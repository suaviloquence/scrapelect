use std::collections::BTreeMap;

use super::{ElementContext, TryFromValue, Value};

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

#[allow(unused_imports)]
pub mod prelude {
    pub use super::{Args, Filter};
    pub use crate::interpreter::{ElementContext, TryFromValue, Value};
    pub use scraper::ElementRef;
    pub use std::{collections::BTreeMap, sync::Arc};
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
fn attrs<'doc>(value: ElementRef<'doc>) -> anyhow::Result<Value<'doc>> {
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

macro_rules! dispatch {
    ($id: ident, $value:ident, $args:ident, $ctx:ident) => {
        $id::Filter::apply($value.try_into()?, Args::try_deserialize($args)?, $ctx)
    };
}

pub fn dispatch_filter<'ast, 'doc>(
    name: &str,
    value: Value<'doc>,
    args: BTreeMap<&'ast str, Value<'doc>>,
    ctx: &mut ElementContext<'ast, 'doc>,
) -> anyhow::Result<Value<'doc>> {
    match name {
        "dbg" => dispatch!(dbg, value, args, ctx),
        "tee" => dispatch!(tee, value, args, ctx),
        "strip" => dispatch!(strip, value, args, ctx),
        "attrs" => dispatch!(attrs, value, args, ctx),
        "take" => dispatch!(take, value, args, ctx),
        other => anyhow::bail!("unrecognized filter `{other}`"),
    }
}
