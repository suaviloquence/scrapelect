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

    fn apply<'ast, 'doc, 'ctx>(
        value: Self::Value<'doc>,
        args: Self::Args<'doc>,
        ctx: &mut ElementContext<'ast, 'doc, 'ctx>,
    ) -> anyhow::Result<Value<'doc>>;
}

macro_rules! mk_filter {
    ($name:ident (value: $val:ty, #[argt] $argt:ty $(=> #[argtt] $argtt:item)?) $fn:item) => {
        mod $name {
            #[allow(unused_imports)]
            use super::{Args, ElementContext, Value};
            #[allow(unused_imports)]
            use std::borrow::Cow;
            #[allow(unused_imports)]
            use scraper::ElementRef;

            #[derive(Debug)]
            pub struct Filter;

            $($argtt)?

            impl super::Filter for Filter {
                type Args<'doc> = $argt;
                type Value<'doc> = $val;

                $fn
            }
        }
    };

    ($name: ident (value: $val:ty$(, $arg:ident: $ty:ty)+) $fn:item) => {
        mk_filter! {
            $name (
                value: $val,
                #[argt] MyArgs<'doc> =>
                #[argtt]
                #[derive(Debug, super::Args)]
                pub struct MyArgs<'doc> {
                    $(
                        $arg: $ty,
                    )+
                }
            )
            $fn
        }
    };

    ($name:ident (value: $val:ty) $fn:item) => {
        mk_filter! {
            $name (
                value: $val,
                #[argt] ()
            ) $fn
        }
    }
}

mk_filter! {
    dbg(value: Value<'doc>, msg: Option<Cow<'doc, str>>)

    fn apply<'ast, 'doc, 'ctx>(
        value: Self::Value<'doc>,
        args: Self::Args<'doc>,
        _: &mut ElementContext<'ast, 'doc, 'ctx>
    ) -> anyhow::Result<Value<'doc>> {
        eprintln!("{}: {}", args.msg.as_deref().unwrap_or("debug message"), value);

        Ok(value)
    }
}

mk_filter! {
    tee(value: Value<'doc>, into: Cow<'doc, str>)

    fn apply<'ast, 'doc, 'ctx>(
        value: Self::Value<'doc>,
        args: Self::Args<'doc>,
        ctx: &mut ElementContext<'ast, 'doc, 'ctx>,
    ) -> anyhow::Result<Value<'doc>> {
        ctx.variables.insert(args.into.into_owned().into(), value.clone());

        Ok(value)
    }
}

mk_filter! {
    strip(value: Cow<'doc, str>)

    fn apply<'ast, 'doc, 'ctx>(
        value: Self::Value<'doc>,
        _: Self::Args<'doc>,
        _: &mut ElementContext<'ast, 'doc, 'ctx>,
    ) -> anyhow::Result<Value<'doc>> {
        let cow = match value {
            Cow::Borrowed(b) => Cow::Borrowed(b.trim()),
            Cow::Owned(s) => Cow::Owned(s.trim().to_owned()),
        };

        Ok(Value::String(cow))
    }
}

macro_rules! dispatch {
    ($id: ident, $value:ident, $args:ident, $ctx:ident) => {
        $id::Filter::apply($value.try_into()?, Args::try_deserialize($args)?, $ctx)
    };
}

pub fn dispatch_filter<'ast, 'doc, 'ctx>(
    name: &str,
    value: Value<'doc>,
    args: BTreeMap<&'ast str, Value<'doc>>,
    ctx: &mut ElementContext<'ast, 'doc, 'ctx>,
) -> anyhow::Result<Value<'doc>> {
    match name {
        "dbg" => dispatch!(dbg, value, args, ctx),
        "tee" => dispatch!(tee, value, args, ctx),
        "strip" => dispatch!(strip, value, args, ctx),
        other => anyhow::bail!("unrecognized filter `{other}`"),
    }
}
