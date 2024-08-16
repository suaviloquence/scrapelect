#![forbid(unsafe_code)]
#![allow(clippy::module_name_repetitions)]
//! API types for making filters for [scrapelect](https://github.com/suaviloquence/scrapelect).
//!
//! # Value
//!
//! This crate provides the value type [`Value`] and its context extensions [`Pipeline`] and
//! [`Element`].  Also provides the trait [`TryFromValue`] to unwrap a [`Value`]
//! to a type it's implemented for.
//!
//! # Errors
//!
//! Provides the [`Error`] enum to handle and create errors, including other
//! Rust errors with [`other!`], [`MessageExt`] and [`WrapExt`].
//!
//! # Context
//!
//! Provides the [`ElementContext`] struct for reading and manipulating state in
//! the scope of an element context block.
//!
//! # Filters
//!
//! Provides the typed [`Filter`] trait and object-safe [`FilterDyn`] one, as well
//! as the [`filter_fn`] macro for easily implementing a typed [`Filter`] on a
//! function.

#[macro_use]
mod error;
mod context;
mod filter;
mod value;

pub use context::{Bindings, ElementContext};
pub use error::{Error, MessageExt, Result, WrapExt};
pub use filter::{filter_fn, Args, Filter, FilterDyn};
pub use value::{
    Data, EValue, Element, ListIter, Number, PValue, Pipeline, StructIter, TryFromValue, Value,
};
