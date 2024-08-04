use std::{iter, option, vec};

use crate::frontend::ast::Qualifier;

use anyhow::Context;
use ExecutionMode::{Collection, One, Optional};

use super::Value;

/// Whether we are matching a list, singular item, or optional item
/// as specified by the user
#[derive(Debug, Clone)]
pub enum ExecutionMode<T> {
    One(T),
    Optional(Option<T>),
    Collection(Vec<T>),
}

#[derive(Debug)]
pub enum IntoIter<T> {
    One(iter::Once<T>),
    Optional(option::IntoIter<T>),
    Collection(vec::IntoIter<T>),
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::One(x) => x.next(),
            Self::Optional(x) => x.next(),
            Self::Collection(x) => x.next(),
        }
    }
}

impl<T> IntoIterator for ExecutionMode<T> {
    type Item = T;

    type IntoIter = IntoIter<T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        match self {
            One(x) => IntoIter::One(iter::once(x)),
            Optional(x) => IntoIter::Optional(x.into_iter()),
            Collection(x) => IntoIter::Collection(x.into_iter()),
        }
    }
}

impl<T> ExecutionMode<T> {
    pub fn hinted_from_iter<I: Iterator<Item = T>>(
        ops: Qualifier,
        mut iter: I,
    ) -> anyhow::Result<Self> {
        Ok(match ops {
            // TODO: take the first, or fail if there are > 1?
            Qualifier::One => One(iter.next().context("Expected exactly one value")?),
            Qualifier::Optional => Optional(iter.next()),
            Qualifier::Collection => Collection(iter.collect()),
        })
    }
}

impl ExecutionMode<Value> {
    pub fn into_value(self) -> Value {
        match self {
            One(x) | Optional(Some(x)) => x,
            Optional(None) => Value::Null,
            Collection(l) => Value::List(l),
        }
    }
}
