use std::{
    collections::BTreeMap,
    sync::{Arc, LazyLock},
};

use anyhow::Context as _;

use crate::interpreter::value::ListIter;

use super::{
    value::{EValue, PValue, Pipeline, TryFromValue},
    ElementContext, Value,
};

pub use scrapelect_filter_proc_macro::{filter_fn, Args};

type Structure<'doc> = BTreeMap<Arc<str>, PValue<'doc>>;

pub trait Args<'doc>: Sized {
    fn try_deserialize<'ast>(args: BTreeMap<&'ast str, EValue<'doc>>) -> anyhow::Result<Self>;
}

impl<'a> Args<'a> for () {
    fn try_deserialize<'ast>(args: BTreeMap<&'ast str, EValue<'a>>) -> anyhow::Result<Self> {
        if !args.is_empty() {
            anyhow::bail!("Found unexpected arguments `{args:?}`");
        }

        Ok(())
    }
}

pub trait Filter {
    type Value<'doc>: TryFromValue<Pipeline<'doc>>;
    type Args<'doc>: Args<'doc>;

    fn apply<'doc>(
        value: Self::Value<'doc>,
        args: Self::Args<'doc>,
        ctx: &mut ElementContext<'_, 'doc>,
    ) -> anyhow::Result<PValue<'doc>>;
}

pub trait FilterDyn {
    fn apply<'ast, 'doc>(
        &self,
        value: PValue<'doc>,
        args: BTreeMap<&'ast str, EValue<'doc>>,
        ctx: &mut ElementContext<'ast, 'doc>,
    ) -> anyhow::Result<PValue<'doc>>;
}

impl<F: Filter> FilterDyn for F {
    #[inline]
    fn apply<'ast, 'doc>(
        &self,
        value: PValue<'doc>,
        args: BTreeMap<&'ast str, EValue<'doc>>,
        ctx: &mut ElementContext<'ast, 'doc>,
    ) -> anyhow::Result<PValue<'doc>> {
        F::apply(value.try_unwrap()?, F::Args::try_deserialize(args)?, ctx)
    }
}

#[filter_fn]
fn id<'doc>(value: PValue<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(value)
}

#[filter_fn]
fn dbg<'doc>(value: PValue<'doc>, msg: Option<Arc<str>>) -> anyhow::Result<PValue<'doc>> {
    let value: EValue = value.into();
    eprintln!("{}: {}", value, msg.as_deref().unwrap_or("dbg message"));

    Ok(value.into())
}

#[filter_fn]
fn tee<'doc>(
    value: PValue<'doc>,
    into: Arc<str>,
    ctx: &mut ElementContext<'_, 'doc>,
) -> anyhow::Result<PValue<'doc>> {
    let value: EValue = value.into();
    ctx.set_var(into.to_string().into(), value.clone())?;
    Ok(value.into())
}

#[filter_fn]
fn strip<'doc>(value: Arc<str>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::String(value.trim().into()))
}

#[filter_fn]
fn attrs<'doc>(value: scraper::ElementRef<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Structure(
        value
            .value()
            .attrs()
            .map(|(k, v)| (Arc::from(k), Value::String(Arc::from(v))))
            .collect(),
    ))
}

#[filter_fn]
fn take<'doc>(mut value: Structure<'doc>, key: Arc<str>) -> anyhow::Result<PValue<'doc>> {
    Ok(value.remove(&key).unwrap_or(Value::Null))
}

#[filter_fn]
fn int<'doc>(value: PValue<'doc>) -> anyhow::Result<PValue<'doc>> {
    let n = match value {
        Value::Int(n) => n,
        Value::Float(x) => x as i64,
        Value::String(s) => s
            .parse()
            .with_context(|| format!("`{s}` is not an integer."))?,
        _ => anyhow::bail!("expected an int, float, or string"),
    };

    Ok(Value::Int(n))
}

#[filter_fn]
fn float<'doc>(value: PValue<'doc>) -> anyhow::Result<PValue<'doc>> {
    let x = match value {
        Value::Int(n) => n as f64,
        Value::Float(x) => x,
        Value::String(s) => s
            .parse()
            .with_context(|| format!("`{s}` is not a float."))?,
        _ => anyhow::bail!("expected an int, float, or string"),
    };

    Ok(Value::Float(x))
}

#[filter_fn]
fn nth<'doc>(mut value: ListIter<'doc>, i: i64) -> anyhow::Result<PValue<'doc>> {
    match value.nth(i.try_into().context("negative indices are not supported")?) {
        Some(x) => Ok(x),
        None => anyhow::bail!("No element at index {i}"),
    }
}

#[filter_fn]
fn keys<'doc>(value: Structure<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Extra(Pipeline::ListIter(Box::new(
        value.into_keys().map(Value::String),
    ))))
}

#[filter_fn]
fn values<'doc>(value: Structure<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Extra(Pipeline::ListIter(Box::new(
        value.into_values(),
    ))))
}

#[filter_fn]
fn and<'doc>(value: bool, with: bool) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(value && with))
}

#[filter_fn]
fn or<'doc>(value: bool, with: bool) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(value || with))
}

#[filter_fn]
fn not<'doc>(value: bool) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(!value))
}

#[filter_fn]
fn split<'doc>(value: Arc<str>, on: Option<Arc<str>>) -> anyhow::Result<PValue<'doc>> {
    if let Some(delim) = on {
        Ok(Value::List(
            value
                .split(&*delim)
                .map(|x| Value::String(Arc::from(x)))
                .collect(),
        ))
    } else {
        Ok(Value::List(
            value
                .split_whitespace()
                .map(|x| Value::String(Arc::from(x)))
                .collect(),
        ))
    }
}

#[filter_fn]
fn eq<'doc>(value: PValue<'doc>, to: EValue<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(EValue::from(value) == to))
}

#[filter_fn]
fn is_in<'doc>(value: PValue<'doc>, list: Vec<EValue<'doc>>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(list.contains(&value.into())))
}

macro_rules! build_map {
    ($(
        $id: ident,
    )*) => {
        [$(
            (stringify!($id), Box::new($id()) as Box<dyn FilterDyn + Send + Sync>),

        )*]
    };
}

static BUILTIN_FILTERS: LazyLock<BTreeMap<&'static str, Box<dyn FilterDyn + Send + Sync>>> =
    LazyLock::new(|| {
        build_map! {
            dbg,
            tee,
            strip,
            take,
            attrs,
            int,
            float,
            nth,
            keys,
            values,
            and,
            or,
            not,
            split,
            eq,
            is_in,
        }
        .into_iter()
        .collect()
    });

pub fn dispatch_filter<'ast, 'doc>(
    name: &str,
    value: PValue<'doc>,
    args: BTreeMap<&'ast str, EValue<'doc>>,
    ctx: &mut ElementContext<'ast, 'doc>,
) -> anyhow::Result<PValue<'doc>> {
    match BUILTIN_FILTERS.get(name) {
        Some(filter) => filter.apply(value, args, ctx),
        None => anyhow::bail!("unrecognized filter `{name}`"),
    }
}
