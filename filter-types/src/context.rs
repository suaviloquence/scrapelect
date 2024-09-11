use std::{borrow::Cow, collections::BTreeMap, sync::Arc};

use serde::Serialize;
use url::Url;

use crate::{Data, EValue, Element, MessageExt, Result, Value};

/// A reference to a parsed HTML element.
pub use scraper::ElementRef;

/// A view into an **element context block**, with setting and retrieving (scoped)
/// bindings, and retrieving properties about this block like a URL and element.
///
/// This trait is **object safe**.  For interacting with entering/exiting different scopes,
/// also implement the non-object-safe [`ElementContext`] trait.
pub trait ElementContextView<'ast, 'ctx> {
    /// Sets the binding with name `name` to `value` in this context.
    ///
    /// Overwrites a previous value bound to that name, if one is present.
    ///
    /// Implementors should not implement this, instead [`set_inner`](Self::set_inner).
    ///
    /// # Errors
    ///
    /// Returns an `Err` if `name` cannot be rebound (e.g., it is `"element"`).
    fn set(&mut self, name: Cow<'ast, str>, value: EValue<'ctx>) -> Result<()> {
        match &*name {
            immutable @ "element" => {
                bail!("assignment to immutable binding `{immutable}`")
            }
            _ => self.set_inner(name, value),
        }
    }

    /// Sets the binding with name `name` to `value` in this context.
    ///
    /// Overwrites a previous value bound to that name, if one is present.
    ///
    /// Implementors should implement this and not `set`.
    fn set_inner(&mut self, name: Cow<'ast, str>, value: EValue<'ctx>) -> Result<()>;

    /// Gets the binding with name `id`, if it is present. Handles
    /// retrieving special bindings like `element`.  Looks in this
    /// context and all parent contexts, starting innermost first.
    ///
    /// Implementors should not implement this and instead use [`get_inner`](Self::get_inner).
    ///
    /// # Errors
    ///
    /// Returns an `Err` if a binding with name `id` is not found in this
    /// scope or any parent scopes.
    fn get(&self, id: &str) -> Result<EValue<'ctx>> {
        match id {
            "element" => Ok(self.element().into()),
            _ => self.get_inner(id),
        }
    }

    /// For implementors of [`ElementContext`].  Only needs to get/set safe
    /// (non-special) bindings, looking up in parent scopes as necessary.
    ///
    /// # Errors
    ///
    /// Returns an `Err` if a binding with name `id` is not found in this
    /// scope or any parent scopes.
    fn get_inner(&self, id: &str) -> Result<EValue<'ctx>>;

    /// Returns a [reference](ElementRef) to the root element of this block.
    #[must_use]
    fn element(&self) -> ElementRef<'ctx>;

    /// Returns a reference to the URL of the document that this element is in.
    #[must_use]
    fn url(&self) -> &Url;
}

/// An expansion of [`ElementContextView`] for interacting with entering and exiting
/// different levels of the scope.  This trait is **not object safe**.
pub trait ElementContext<'ast, 'ctx>: ElementContextView<'ast, 'ctx> {
    /// A type representing an inner context from [`Self::nest`].  This is most often
    /// `Self<'ast, 'newctx>` but we need this associated type to express HKTs like that.
    type Nested<'inner>: ElementContext<'ast, 'inner>
    where
        // implies 'ast: 'inner and 'ctx: 'inner
        Self: 'inner;

    /// Creates a new top-level instance with the given URL and element reference.
    #[must_use]
    fn new(element: ElementRef<'ctx>, url: Url) -> Self;

    /// Consumes the element context, returning key: value [`Bindings`]
    ///
    /// This is a **lossy** operation, dropping any statements that refer to the element
    /// itself.
    #[must_use]
    fn into_bindings(self) -> Bindings<'ast>;

    /// Enter a new, nested element context level with the given url (if different from the parent) and element reference.
    #[must_use]
    fn nest<'inner, 'outer: 'inner>(
        &'outer self,
        url: Option<Url>,
        element: ElementRef<'inner>,
    ) -> Self::Nested<'inner>;
}

/// Holds state for an **element context block**, including the
/// parent block, if any.
#[derive(Debug)]
pub struct Linked<'ast, 'ctx> {
    /// The current name-value bindings mapping at this level.
    pub bindings: Bindings<'ast, Element<'ctx>>,
    /// The [`ElementRef`] pointing to the parsed HTML element.
    pub element: ElementRef<'ctx>,
    /// A reference to the parent scope, if there is one.
    pub parent: Option<&'ctx Self>,
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

impl<'ast, 'ctx> Linked<'ast, 'ctx> {
    /// Creates a new [`Linked`] instance with the given `element`,
    /// `parent` reference (if it is `Some`), and `url`.
    ///
    /// Creates an empty [`Bindings`] map for this context.
    #[must_use]
    pub fn new(element: ElementRef<'ctx>, parent: Option<&'ctx Self>, url: Url) -> Self {
        Self {
            bindings: Bindings::new(),
            element,
            parent,
            url,
        }
    }
}

impl<'ast, 'ctx> ElementContextView<'ast, 'ctx> for Linked<'ast, 'ctx> {
    fn get_inner(&self, id: &str) -> Result<EValue<'ctx>> {
        match self.bindings.0.get(id) {
            Some(id) => Ok(id.clone()),
            None => self
                .parent
                .with_msg(|| format!("unknown binding `{id}`"))?
                .get(id),
        }
    }

    #[inline]
    fn set_inner(&mut self, name: Cow<'ast, str>, value: EValue<'ctx>) -> Result<()> {
        self.bindings.0.insert(name, value);
        Ok(())
    }

    #[inline]
    fn url(&self) -> &Url {
        &self.url
    }

    #[inline]
    fn element(&self) -> ElementRef<'ctx> {
        self.element
    }
}

impl<'ast, 'ctx> ElementContext<'ast, 'ctx> for Linked<'ast, 'ctx> {
    type Nested<'inner> = Linked<'ast, 'inner> where Self: 'inner;

    #[inline]
    fn new(element: ElementRef<'ctx>, url: Url) -> Self {
        Self::new(element, None, url)
    }

    #[inline]
    fn into_bindings(self) -> Bindings<'ast> {
        self.bindings.into_data()
    }

    fn nest<'inner, 'outer: 'inner>(
        &'outer self,
        url: Option<Url>,
        element: ElementRef<'inner>,
    ) -> Self::Nested<'inner> {
        Linked::new(element, Some(self), url.unwrap_or_else(|| self.url.clone()))
    }
}
