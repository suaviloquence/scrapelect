use std::{future::Future, iter, option, vec};

use crate::frontend::ast::SelectorOpts;

use super::DataValue;
use anyhow::Context;
use ExecutionMode::{Collection, One, Optional};

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
    pub fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> ExecutionMode<U> {
        match self {
            One(x) => One(f(x)),
            Optional(Some(x)) => Optional(Some(f(x))),
            Optional(None) => Optional(None),
            Collection(l) => Collection(l.into_iter().map(f).collect()),
        }
    }

    #[inline]
    pub async fn async_map<U, Fut: Future<Output = U>, Fn: FnMut(T) -> Fut>(
        self,
        f: Fn,
    ) -> ExecutionMode<U> {
        self.map(f).transpose_fut().await
    }

    pub fn hinted_from_iter<I: Iterator<Item = T>>(
        ops: SelectorOpts,
        mut iter: I,
    ) -> anyhow::Result<Self> {
        Ok(match ops {
            // TODO: take the first, or fail if there are > 1?
            SelectorOpts::One => One(iter.next().context("Expected exactly one value")?),
            SelectorOpts::Optional => Optional(iter.next()),
            SelectorOpts::Collection => Collection(iter.collect()),
        })
    }
}

impl ExecutionMode<DataValue> {
    pub fn into_data_value(self) -> DataValue {
        match self {
            One(x) | Optional(Some(x)) => x,
            Optional(None) => DataValue::Null,
            Collection(l) => DataValue::List(l),
        }
    }
}

impl<T, E> ExecutionMode<Result<T, E>> {
    pub fn transpose_res(self) -> Result<ExecutionMode<T>, E> {
        Ok(match self {
            One(x) => One(x?),
            Optional(Some(x)) => Optional(Some(x?)),
            Optional(None) => Optional(None),
            Collection(l) => Collection(l.into_iter().collect::<Result<_, E>>()?),
        })
    }
}

impl<T, F: Future<Output = T>> ExecutionMode<F> {
    pub async fn transpose_fut(self) -> ExecutionMode<T> {
        match self {
            One(f) => One(f.await),
            Optional(Some(f)) => Optional(Some(f.await)),
            Optional(None) => Optional(None),
            Collection(l) => Collection(futures::future::join_all(l).await),
        }
    }
}
