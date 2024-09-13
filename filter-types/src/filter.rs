use std::collections::BTreeMap;

use super::{
    value::{EValue, PValue, Pipeline, TryFromValue},
    ElementContextView, Result,
};

pub use scrapelect_filter_proc_macro::{filter_fn, Args};

/// Typed arguments for a [`Filter`].
///
/// If all the fields in the struct implement [`TryFromValue<Element`], it is easier
/// to use the derive macro [`Args`], but this can also be implemented manually if
/// you need to express more custom deserializing logic.
///
/// [`Args`](scrapelect_filter_proc_macro::Args)
pub trait Args<'doc>: Sized {
    /// Try to deserialize the typed arguments from the given `args`.
    ///
    /// # Errors
    ///
    /// The arguments are invalid to deserialize to this structure.
    fn try_deserialize<'ast>(args: BTreeMap<&'ast str, EValue<'doc>>) -> Result<Self>;
}

impl<'a> Args<'a> for () {
    fn try_deserialize<'ast>(args: BTreeMap<&'ast str, EValue<'a>>) -> Result<Self> {
        if !args.is_empty() {
            bail!("Found unexpected arguments `{args:?}`");
        }

        Ok(())
    }
}

/// A typed filter, callable with the given arguments in `apply`.
///
/// The easiest way to create this is with [`filter_fn`](scrapelect_filter_proc_macro::filter_fn).
///
/// However, it can be implemented manually if necessary.
pub trait Filter {
    /// The `value` type; the type of the input value to the filter call in
    /// `value | call(args...)`.
    ///
    /// It must implement [`TryFromValue`] for [`Pipeline`] values.  This is implemented
    /// for unwrapped variants of `Value` but you can provide your own implementation on
    /// a newtype if necessary.
    type Value<'doc>: TryFromValue<Pipeline<'doc>>;
    /// The type of the arguments for this filter; see [`Args`]
    type Args<'doc>: Args<'doc>;

    /// Call this filter with the given `value`, `args`, and `ctx`, returning
    /// the modified value by the filter call or an error.
    ///
    /// # Errors
    ///
    /// An implementor should return `Err` if the filter was called with invalid
    /// arguments, or if the filter cannot be called for some reason.
    fn apply<'ast, 'doc, E: ElementContextView<'ast, 'doc> + ?Sized>(
        value: Self::Value<'doc>,
        args: Self::Args<'doc>,
        ctx: &mut E,
    ) -> Result<PValue<'doc>>;
}

/// An object-safe version of [`Filter`]. All `F: Filter` implement this trait,
/// so prefer implementing `Filter` unless you must:
///
/// 1. Deserialize the input [`PValue`] in a custom way (not using [`TryFromValue`])
/// 2. Use custom arg-deserializing logic (but often you will be able to implement
///    [`Args`] manually instead, and still get the typed guarantees of [`Filter`])
/// 3. Use the `&self` reference.  This *can* be used to store state with interior
///    mutability (though note that in `scrapelect`, filters must be `Send + Sync`) to
///    register, but it is often not the best idea to have filter state because filters
///    can be called from anywhere in the program, and you will have to reason out the
///    soundness of having the state.
pub trait FilterDyn {
    /// Call this filter with the given `value`, `args`, and `ctx`.
    ///
    /// # Errors
    ///
    /// Implementors should return an `Err` if the filter call is invalid.
    fn apply<'ast, 'doc>(
        &self,
        value: PValue<'doc>,
        args: BTreeMap<&'ast str, EValue<'doc>>,
        ctx: &mut dyn ElementContextView<'ast, 'doc>,
    ) -> Result<PValue<'doc>>;
}

impl<F: Filter> FilterDyn for F {
    #[inline]
    fn apply<'ast, 'doc>(
        &self,
        value: PValue<'doc>,
        args: BTreeMap<&'ast str, EValue<'doc>>,
        ctx: &mut dyn ElementContextView<'ast, 'doc>,
    ) -> Result<PValue<'doc>> {
        F::apply(value.try_unwrap()?, F::Args::try_deserialize(args)?, ctx)
    }
}
