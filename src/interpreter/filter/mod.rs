use std::collections::BTreeMap;

use super::{
    value::{EValue, PValue, Pipeline, TryFromValue},
    ElementContext,
};

pub mod builtin;

pub use scrapelect_filter_proc_macro::{filter_fn, Args};

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

#[cfg(not(feature = "filter_doc"))]
pub fn dispatch_filter<'ast, 'doc>(
    name: &str,
    value: PValue<'doc>,
    args: BTreeMap<&'ast str, EValue<'doc>>,
    ctx: &mut ElementContext<'ast, 'doc>,
) -> anyhow::Result<PValue<'doc>> {
    match builtin::FILTERS.get(name) {
        Some(filter) => filter.apply(value, args, ctx),
        None => anyhow::bail!("unrecognized filter `{name}`"),
    }
}
