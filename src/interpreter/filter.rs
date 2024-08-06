#![cfg_attr(
    feature = "filter_doc",
    doc = "Documentation for `scrapelect`'s built-in filters.

Conventions used:

- Signature of a filter: `value: T | name(arg_1: U_1, ...): V` means that `name` is a filter
    that takes a *pipeline value* of type `T`, has arguments `arg_i` of type `U_i`, and returns
    a value of type `V`
  - Specifying an arg type with a question mark (e.g., `value: Value | dbg(msg: String?): Value`)
    means that that argument (e.g., `msg`) is *optional* and can be omitted.
- List shorthand: a `List` is represented as `[a_0, a_1, a_2, ..., a_n]` to mean that
its elements are `a_0, ..., a_n` in that order.  Indexing starts at 0.  This syntax is currently
not valid `scrapelect`, but it is useful to express in documentation.
- Structure shorthand: similarly, a structure is represented as { key_1: value_1, key_2: value_2, ... } to
indicate that it has keys that correspond to the given values. This is not valid `scrapelect`,
but it is useful in documentation.
- Element shorthand: an inline HTML element (e.g., `<a href=\"github.com/suaviloquence/scrapelect\">Link text</a>`)
   is also not valid `scrapelect`, but is useful to demonstrate how `Element`s are used
   in filters.
"
)]

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

/// Signature: `value | id(): Value`
///
/// Returns the `value` passed into it.
///
/// # Examples
///
/// - `"hi" | id()` returns `"hi"`
#[filter_fn]
pub fn id<'doc>(value: PValue<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(value)
}

/// Signature: `value | dbg(msg: String?): Value`
///
/// Returns the `value` passed into it, printing a copy to the console,
/// prepended with `msg` if specified.
///
/// # Examples
///
/// - `"hi" | dbg()` returns `"hi"` and prints `dbg message: "hi"`
/// - `"hi" | dbg(msg: "I say")` returns `"hi"` and prints `I say: "hi"`
#[filter_fn]
pub fn dbg<'doc>(value: PValue<'doc>, msg: Option<Arc<str>>) -> anyhow::Result<PValue<'doc>> {
    let value: EValue = value.into();
    eprintln!("{}: {}", value, msg.as_deref().unwrap_or("dbg message"));

    Ok(value.into())
}

/// Signature: `value | tee(into: String): Value`
///
/// Returns the `value` passed into it, saving a copy into the
/// variable with name `into` in the current scope.
///
/// # Examples
///
/// - `"abcde" | tee(into: "tmp")` returns `"abcde"`, and `$tmp` is also `"abcde"`
/// - Chaining filters (see [`strip`]):
///   `stripped: "   hi   " | tee(into: "spacey") | strip()` results in:
///   - `$spacey = "   hi   "`
///   - `$stripped = "hi"`
#[filter_fn]
pub fn tee<'doc>(
    value: PValue<'doc>,
    into: Arc<str>,
    ctx: &mut ElementContext<'_, 'doc>,
) -> anyhow::Result<PValue<'doc>> {
    let value: EValue = value.into();
    ctx.set_var(into.to_string().into(), value.clone())?;
    Ok(value.into())
}

/// Signature: `value: String | strip(): String`
///
/// Removes leading and trailing whitespace from a string.
///
/// # Examples
///
/// - `"    helloooo   ooo   " | strip()` returns `"helloooo   ooo"`
/// - `"    " | strip()` returns `""`
#[filter_fn]
pub fn strip<'doc>(value: Arc<str>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::String(value.trim().into()))
}

/// Signature: `value: Element | attrs(): Structure`
///
/// Returns the attributes of an element as key-value records in the structure.
///
/// Note that all values will be strings, use filters like [`int`] to convert them if needed.
///
/// # Examples
///
/// - `<img src="./cat.png" alt="a Kitty Cat" /> | attrs()` returns `{ src: "./cat.png", alt: "a Kitty Cat" }`
/// - `<p>Hello!</p> | attrs()` returns `{}`
#[filter_fn]
pub fn attrs<'doc>(value: scraper::ElementRef<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Structure(
        value
            .value()
            .attrs()
            .map(|(k, v)| (Arc::from(k), Value::String(Arc::from(v))))
            .collect(),
    ))
}

/// Signature: `value: Structure | take(key: String): Value`
///
/// Returns the value at key `key` in the structure.  If there is no value there,
/// returns `Null`.
///
/// # Examples
///
/// - `{ kitty: "cat" } | take(key: "kitty")` returns `"cat"`
/// - `{ kitty: "cat" } | take(key: "cat")` returns `null`
#[filter_fn]
pub fn take<'doc>(mut value: Structure<'doc>, key: Arc<str>) -> anyhow::Result<PValue<'doc>> {
    Ok(value.remove(&key).unwrap_or(Value::Null))
}

/// Signature: `value: (String or Int or Float) | int(): Int`
///
/// Turns the value into an `Int`.  If it is a String, it must be a valid
/// representation of an interger; if not it will raise an error.  If a `Float`
/// is passed to this filter, it will truncate (round down) to the nearest integer.
///
/// # Examples
///
/// - `1 | int()` returns `1`
/// - `1.5 | int()` returns `1`
/// - `"1" | int()` returns `1`
/// - `">_<" | int()` raises an error.
#[filter_fn]
pub fn int<'doc>(value: PValue<'doc>) -> anyhow::Result<PValue<'doc>> {
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

/// Signature: `value: (String or Int or Float) | float(): Float`
///
/// Turns the value into a `Float`.  If it is a String, it must be a valid
/// representation of a float; if not it will raise an error.
///
/// # Examples
///
/// - `1 | float()` returns `1.0`
/// - `1.5 | float()` returns `1.5`
/// - `"1.5" | float()` returns `1.5`
/// - `">_<" | float()` raises an error.
#[filter_fn]
pub fn float<'doc>(value: PValue<'doc>) -> anyhow::Result<PValue<'doc>> {
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

/// Signature: `value: List | nth(i: Int): Value`
///
/// Obtains the `i`th element in the list `value`, starting from zero.
///
/// Negative indices or indices >= the `length` of the list are invalid arguments
/// and will raise an error.
///
/// # Examples
///
/// - `[1, 2, 3, 4] | nth(i: 0)` returns `1`
/// - `[1, 2, 3, 4] | nth(i: 4)` raises an out-of-bounds error.
#[filter_fn]
pub fn nth<'doc>(mut value: ListIter<'doc>, i: i64) -> anyhow::Result<PValue<'doc>> {
    match value.nth(i.try_into().context("negative indices are not supported")?) {
        Some(x) => Ok(x),
        None => anyhow::bail!("No element at index {i}"),
    }
}

/// Signature: `value: Structure | keys(): List<String>`
///
/// Turns the structure `value` into a list of the *keys* of the structure.
///
/// Output is in alphabetical order.
///
/// # Examples
///
/// - `{a: 1, b: "hello"} | values()` returns `["a", "b"]`
/// - `{} | values()` returns `[]`
#[filter_fn]
pub fn keys<'doc>(value: Structure<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Extra(Pipeline::ListIter(Box::new(
        value.into_keys().map(Value::String),
    ))))
}

/// Signature: `value: Structure | values(): List`
///
/// Turns the structure `value` into a list of the *values* of the structure.
///
/// Output is in alphabetical order by key, duplicates are included.
///
/// # Examples
///
/// - `{a: 1, b: "hello"} | values()` returns `[1, "hello"]`
/// - `{} | values()` returns `[]`
#[filter_fn]
pub fn values<'doc>(value: Structure<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Extra(Pipeline::ListIter(Box::new(
        value.into_values(),
    ))))
}

/// Signature: `value: Bool | and(with: Bool): Bool`
///
/// Returns the boolean AND of `value` and `with`: `true` if both
/// (or both) of `value` and `and` are true.
///
/// # Examples
///
/// - `true | or(with: true)` returns `true`
/// - `false | or(with: true)` returns `false`
#[filter_fn]
pub fn and<'doc>(value: bool, with: bool) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(value && with))
}

/// Signature: `value: Bool | or(with: Bool): Bool`
///
/// Returns the boolean OR of `value` and `with`: `true` if either
/// (or both) of `value` and `and` is true.
///
/// # Examples
///
/// - `true | or(with: false)` returns `true`
/// - `false | or(with: false)` returns `false`
#[filter_fn]
pub fn or<'doc>(value: bool, with: bool) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(value || with))
}

/// Signature: `value: Bool | not(): Bool`
///
/// Returns the boolean NOT (opposite)  of `value`.
///
/// # Examples
///
/// - `true | not()` returns `false`
/// - `false | not()` returns `true`
#[filter_fn]
pub fn not<'doc>(value: bool) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(!value))
}

/// Signature: `value: String | is_in(on: String?): List<String>`
///
/// Splits a `String` into a `List` of substrings, on the specified delimiter `on`
/// or over whitespace if `on` is not set.
///
/// # Examples
///
/// - `"my very excellent mother" | split()` returns `["my", "very", "excellent", "mother"]`
/// - `"my very excellent mother" | split(on: "excellent") returns `["my very ", " mother"]
/// - `"my very excellent mother" | split(on: "HAMPSTERS") returns ["my very excellent mother"]
#[filter_fn]
pub fn split<'doc>(value: Arc<str>, on: Option<Arc<str>>) -> anyhow::Result<PValue<'doc>> {
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

/// Signature: `value | eq(to: Value): Bool`
///
/// Takes a two values `value` and `to`, and returns whether `value` equal to `to`.
///
/// # Examples
///
/// - `2 | eq(to: 2)` is `true`
/// - `6 | eq(to: 2)` is `false`.
#[filter_fn]
pub fn eq<'doc>(value: PValue<'doc>, to: EValue<'doc>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(EValue::from(value) == to))
}

/// Signature: `value | is_in(list: List): Bool`
///
/// Takes a value and a List `list`, and returns whether `value` is in the `List`
///
/// # Examples
///
/// Let `list: [1, 2, 3, 4, 5];`:
///
/// Then `2 | is_in(list: $list)` is `true`, but `6 | is_in(list: $list)` is `false.
#[filter_fn]
pub fn is_in<'doc>(value: PValue<'doc>, list: Vec<EValue<'doc>>) -> anyhow::Result<PValue<'doc>> {
    Ok(Value::Bool(list.contains(&value.into())))
}

/// Signature: `value | truthy(): Bool`
///
/// Takes a `value` and converts it into a `Bool`, based on whether it is "truthy".
///
/// Rules:
///
/// - `Null` is `false`
/// - `Int`s and `Float`s are `true` if they are nonzero.
/// - `String`s, `List`s, and `Structure`s are `true` if they are nonempty
/// - `Element`s are `true`
/// - `Bool`s are the boolean value
///
/// # Examples
///
/// - `"" | truthy()` is `false`
/// - `"hi!" | truthy()` is `true`
/// - `0 | truthy()` is `false`
/// - `0.6931 | truthy()` is `true`
/// - `null | truthy()` is `false`
/// - `[null] | truthy()` is `true` (a List containing `null` is nonempty)
/// - `true | truthy()` is `true`
#[filter_fn]
pub fn truthy<'doc>(value: PValue<'doc>) -> anyhow::Result<PValue<'doc>> {
    let truthy = match value {
        Value::Null => false,
        Value::Float(f) => f != 0.,
        Value::Int(i) => i != 0,
        Value::Bool(b) => b,
        Value::String(s) => !s.is_empty(),
        Value::List(l) => !l.is_empty(),
        Value::Structure(s) => !s.is_empty(),
        Value::Extra(Pipeline::Element(_)) => true,
        Value::Extra(Pipeline::ListIter(mut i)) => i.next().is_some(),
        Value::Extra(Pipeline::StructIter(mut i)) => i.next().is_some(),
    };

    Ok(Value::Bool(truthy))
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

#[cfg(not(feature = "filter_doc"))]
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

#[cfg(not(feature = "filter_doc"))]
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
