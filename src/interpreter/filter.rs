use std::{
    collections::BTreeMap,
    sync::{Arc, LazyLock},
};

use anyhow::Context as _;

use super::{value::Or, ElementContext, TryFromValue, Value};

pub use filter_proc_macro::{filter_fn, Args};

pub trait Args<'doc>: Sized {
    fn try_deserialize<'ast>(args: BTreeMap<&'ast str, Value<'doc>>) -> anyhow::Result<Self>;
}

impl<'a> Args<'a> for () {
    fn try_deserialize<'ast>(args: BTreeMap<&'ast str, Value<'a>>) -> anyhow::Result<Self> {
        if !args.is_empty() {
            anyhow::bail!("Found unexpected arguments `{args:?}`");
        }

        Ok(())
    }
}

pub trait Filter {
    type Value<'doc>: TryFromValue<'doc>;
    type Args<'doc>: Args<'doc>;

    fn apply<'doc>(
        value: Self::Value<'doc>,
        args: Self::Args<'doc>,
        ctx: &mut ElementContext<'_, 'doc>,
    ) -> anyhow::Result<Value<'doc>>;
}

pub trait FilterDyn {
    fn apply<'ast, 'doc>(
        &self,
        value: Value<'doc>,
        args: BTreeMap<&'ast str, Value<'doc>>,
        ctx: &mut ElementContext<'ast, 'doc>,
    ) -> anyhow::Result<Value<'doc>>;
}

impl<F: Filter> FilterDyn for F {
    #[inline]
    fn apply<'ast, 'doc>(
        &self,
        value: Value<'doc>,
        args: BTreeMap<&'ast str, Value<'doc>>,
        ctx: &mut ElementContext<'ast, 'doc>,
    ) -> anyhow::Result<Value<'doc>> {
        F::apply(value.try_into()?, F::Args::try_deserialize(args)?, ctx)
    }
}

#[filter_fn]
fn id<'doc>(value: Value<'doc>) -> anyhow::Result<Value<'doc>> {
    Ok(value)
}

#[filter_fn]
fn dbg<'doc>(value: Value<'doc>, msg: Option<Arc<str>>) -> anyhow::Result<Value<'doc>> {
    eprintln!("{}: {}", value, msg.as_deref().unwrap_or("dbg message"));

    Ok(value)
}

#[filter_fn]
fn tee<'doc>(
    value: Value<'doc>,
    into: Arc<str>,
    ctx: &mut ElementContext<'_, 'doc>,
) -> anyhow::Result<Value<'doc>> {
    ctx.set_var(into.to_string().into(), value.clone())?;
    Ok(value)
}

#[filter_fn]
fn strip<'doc>(value: Arc<str>) -> anyhow::Result<Value<'doc>> {
    Ok(Value::String(value.trim().into()))
}

#[filter_fn]
fn attrs<'doc>(value: scraper::ElementRef<'doc>) -> anyhow::Result<Value<'doc>> {
    Ok(Value::Structure(
        value
            .value()
            .attrs()
            .map(|(k, v)| (Arc::from(k), Value::String(Arc::from(v))))
            .collect(),
    ))
}

#[filter_fn]
fn take<'doc>(
    mut value: BTreeMap<Arc<str>, Value<'doc>>,
    key: Arc<str>,
) -> anyhow::Result<Value<'doc>> {
    Ok(value.remove(&key).unwrap_or(Value::Null))
}

#[filter_fn]
fn int<'doc>(value: Or<i64, Or<f64, Arc<str>>>) -> anyhow::Result<Value<'doc>> {
    let n = match value {
        Or::A(n) => n,
        Or::B(Or::A(x)) => x as i64,
        Or::B(Or::B(s)) => s
            .parse()
            .with_context(|| format!("`{s}` is not an integer."))?,
    };

    Ok(Value::Int(n))
}

#[filter_fn]
fn float<'doc>(value: Or<f64, Or<i64, Arc<str>>>) -> anyhow::Result<Value<'doc>> {
    let x = match value {
        Or::A(x) => x,
        Or::B(Or::A(n)) => n as f64,
        Or::B(Or::B(s)) => s
            .parse()
            .with_context(|| format!("`{s}` is not a float."))?,
    };

    Ok(Value::Float(x))
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
        }
        .into_iter()
        .collect()
    });

pub fn dispatch_filter<'ast, 'doc>(
    name: &str,
    value: Value<'doc>,
    args: BTreeMap<&'ast str, Value<'doc>>,
    ctx: &mut ElementContext<'ast, 'doc>,
) -> anyhow::Result<Value<'doc>> {
    match BUILTIN_FILTERS.get(name) {
        Some(filter) => filter.apply(value, args, ctx),
        None => anyhow::bail!("unrecognized filter `{name}`"),
    }
}
