use std::collections::BTreeMap;

use super::{ElementContext, TryFromValue, Value};

pub use filter_proc_macro::Args;

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
mod prelude {
    pub use super::{Args, Filter};
    pub use crate::interpreter::{ElementContext, TryFromValue, Value};
    pub use scraper::ElementRef;
    pub use std::sync::Arc;
}

mod dbg {
    use super::prelude::*;

    #[derive(Debug, Args)]
    pub struct Args {
        msg: Option<Arc<str>>,
    }

    #[derive(Debug)]
    pub struct Filter;

    impl super::Filter for Filter {
        type Value<'doc> = Value<'doc>;

        type Args<'doc> = Args;

        fn apply<'doc>(
            value: Self::Value<'doc>,
            args: Self::Args<'doc>,
            _ctx: &mut ElementContext<'_, 'doc>,
        ) -> anyhow::Result<Value<'doc>> {
            eprintln!(
                "{}: {}",
                value,
                args.msg.as_deref().unwrap_or("dbg message")
            );

            Ok(value)
        }
    }
}

mod tee {
    use std::borrow::Cow;

    use super::prelude::*;

    #[derive(Debug, Args)]
    pub struct Args {
        into: Arc<str>,
    }

    #[derive(Debug)]
    pub struct Filter;

    impl super::Filter for Filter {
        type Value<'doc> = Value<'doc>;

        type Args<'doc> = Args;

        fn apply<'doc>(
            value: Self::Value<'doc>,
            args: Self::Args<'doc>,
            ctx: &mut ElementContext<'_, 'doc>,
        ) -> anyhow::Result<Value<'doc>> {
            ctx.set_var(Cow::from((*args.into).to_owned()), value.clone())?;
            Ok(value)
        }
    }
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
        // "strip" => dispatch!(strip, value, args, ctx),
        other => anyhow::bail!("unrecognized filter `{other}`"),
    }
}
