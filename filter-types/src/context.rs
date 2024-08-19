use std::{borrow::Cow, collections::BTreeMap, sync::Arc};

use serde::Serialize;
use url::Url;

use crate::{Data, EValue, Element, MessageExt, Result, Value};

/// A reference to a parsed HTML element.
pub use scraper::ElementRef;

/// Holds state for an **element context block**, including the
/// parent block, if any.
#[derive(Debug)]
pub struct ElementContext<'ast, 'ctx> {
    /// The current name-value bindings mapping at this level.
    pub bindings: Bindings<'ast, Element<'ctx>>,
    /// The [`ElementRef`] pointing to the parsed HTML element.
    pub element: ElementRef<'ctx>,
    /// A reference to the parent scope, if there is one.
    pub parent: Option<&'ctx ElementContext<'ast, 'ctx>>,
    /// The URL of this context, set by URL recursion if indicated, or the parent's
    /// URL if not.
    pub url: Url,
}

/// Holds a mapping of named bindings to [`Value`]s.
///
/// By default, it uses the [`Data`] extra variant of value, but it can be extension
/// by altering the `X` type parameter.
#[derive(Debug, Serialize)]
pub struct Bindings<'a, X = Data>(pub BTreeMap<Cow<'a, str>, Value<X>>);

impl<'a, X> Bindings<'a, X> {
    /// Creates a new, empty [`Bindings`] instance.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    /// Converts this bindings map to use [`Data`] as the [`Value`] extension.
    ///
    /// This is a **lossy** operation, see [`Value::into_data`].  It removes any
    /// bindings that cannot be turned into bare [`Value`]s.
    #[must_use]
    pub fn into_data(self) -> Bindings<'a, Data> {
        Bindings(
            self.0
                .into_iter()
                .filter_map(|(k, v)| Some((k, v.into_data()?)))
                .collect(),
        )
    }

    /// Converts a [`Bindings<Data>`] into a [`Bindings<X>`].  This is a lossless operation
    /// because a [`Value<Data>`] can always be converted into a [`Value<X>`].
    #[must_use]
    pub fn from_data(bindings: Bindings<'a, Data>) -> Self {
        Self(
            bindings
                .0
                .into_iter()
                .map(|(k, v)| (k, Value::from_data(v)))
                .collect(),
        )
    }

    /// Converts these [`Bindings`] into a [`Value`].  Specifically, it makes
    /// them a key-value `Structure` by taking ownership of the keys and mapping
    /// them to the mapped values.
    #[must_use]
    pub fn into_value(self) -> Value<X> {
        Value::Structure(
            self.0
                .into_iter()
                .map(|(k, v)| (Arc::from(k.into_owned()), v))
                .collect(),
        )
    }
}

impl<'a, X> Default for Bindings<'a, X> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<'ast, 'ctx> ElementContext<'ast, 'ctx> {
    /// Creates a new [`ElementContext`] instance with the given `element`,
    /// `parent` reference (if it is `Some`), and `url`.
    ///
    /// Creates an empty [`Bindings`] map for this context.
    #[must_use]
    pub fn new(
        element: ElementRef<'ctx>,
        parent: Option<&'ctx ElementContext<'ast, 'ctx>>,
        url: Url,
    ) -> Self {
        Self {
            bindings: Bindings::new(),
            element,
            parent,
            url,
        }
    }

    /// Gets the binding with name `id`, if it is present. Handles
    /// retrieving special bindings like `element`.  Looks in this
    /// context and all parent contexts, starting innermost first.
    ///
    /// # Errors
    ///
    /// Returns an `Err` if a binding with name `id` is not found in this
    /// scope or any parent scopes.
    pub fn get(&self, id: &str) -> Result<EValue<'ctx>> {
        match id {
            "element" => Ok(self.element.into()),
            _ => match self.bindings.0.get(id) {
                Some(id) => Ok(id.clone()),
                None => self
                    .parent
                    .with_msg(|| format!("unknown binding `{id}`"))?
                    .get(id),
            },
        }
    }

    /// Sets the binding with name `name` to `value` in this context.
    ///
    /// Overwrites a previous value bound to that name, if one is present.
    ///
    /// # Errors
    ///
    /// Returns an `Err` if `name` cannot be rebound (e.g., it is `"element"`).
    pub fn set(&mut self, name: Cow<'ast, str>, value: EValue<'ctx>) -> Result<()> {
        match &*name {
            immutable @ "element" => {
                bail!("assignment to immutable binding `{immutable}`")
            }
            _ => self.bindings.0.insert(name, value),
        };

        Ok(())
    }
}
