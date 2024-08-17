use std::borrow::Cow;

/// Represents a named selector fragment, like `h2` or `#id`.
#[derive(Debug, Clone)]
pub enum NamedSelector<'a> {
    /// Select by HTML tag name, such as `a` or `h2`.
    Tag(&'a str),
    /// Select by HTML class, such as `.kitty` or `.item`.
    Class(&'a str),
    /// Select by HTML id, such as `#unique` or `#main`.
    Id(&'a str),
}

/// A selector fragment, combinable with [`SelectorCombinator`].
#[derive(Debug, Clone)]
pub enum SelectorFragment<'a> {
    /// An AND of named selector fragments.  The string
    /// `a#b.c` would be `[Tag("a"), Id("b"), Class("c")]`.
    Named(NonEmpty<NamedSelector<'a>>),
    /// The special wildcard selector `*`.
    Any,
}

/// A [`Vec<T>`] that is guaranteed to have at least one element.
#[derive(Debug, Clone)]
pub struct NonEmpty<T>(Vec<T>);

impl<T> NonEmpty<T> {
    #[inline]
    #[must_use]
    pub fn from_one(item: T) -> Self {
        Self(vec![item])
    }

    #[must_use]
    pub fn from_vec(vec: Vec<T>) -> Option<Self> {
        if vec.is_empty() {
            None
        } else {
            Some(Self(vec))
        }
    }

    #[inline]
    #[must_use]
    pub fn first(&self) -> &T {
        assert!(self.0.len() > 0);
        &self[0]
    }

    #[inline]
    #[must_use]
    pub fn last(&self) -> &T {
        assert!(self.0.len() > 0);
        &self[self.0.len() - 1]
    }

    #[inline]
    pub fn push(&mut self, item: T) {
        self.0.push(item);
    }
}

impl<T> std::ops::Deref for NonEmpty<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SelectorCombinator {
    NextSibling,
    Child,
    SubsequentSibling,
    Descendent,
}

/// A full CSS selector, with a nonempty head and a list of selectors.
#[derive(Debug, Clone)]
pub struct Selector<'a> {
    pub head: SelectorFragment<'a>,
    pub combinators: Vec<(SelectorCombinator, SelectorFragment<'a>)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Qualifier {
    #[default]
    One,
    Optional,
    Collection,
}

#[derive(Debug, Clone)]
pub enum Leaf<'a> {
    Var(&'a str),
    Int(i64),
    Float(f64),
    String(Cow<'a, str>),
}

#[derive(Debug, Clone)]
pub enum RValue<'a> {
    Leaf(Leaf<'a>),
    Element(Element<'a>),
}

#[derive(Debug, Clone)]
pub struct Inline<'a> {
    pub value: Leaf<'a>,
    pub filters: Vec<Filter<'a>>,
}

impl<'a> From<Leaf<'a>> for Inline<'a> {
    fn from(value: Leaf<'a>) -> Self {
        Self {
            value,
            filters: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Statement<'a> {
    pub id: &'a str,
    pub value: RValue<'a>,
    pub filters: Vec<Filter<'a>>,
}

#[derive(Debug, Clone)]
pub struct Element<'a> {
    pub url: Option<Inline<'a>>,
    pub selector: Selector<'a>,
    pub qualifier: Qualifier,
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub enum FilterType<'a> {
    Call(FilterCall<'a>),
    Select(FilterSelect<'a>),
}

#[derive(Debug, Clone)]
pub struct Filter<'a> {
    pub filter: FilterType<'a>,
    pub qualifier: Qualifier,
}

#[derive(Debug, Clone)]
pub struct FilterCall<'a> {
    pub id: &'a str,
    pub args: Vec<Arg<'a>>,
}

#[derive(Debug, Clone)]
pub struct FilterSelect<'a> {
    pub name: &'a str,
    pub value: Inline<'a>,
}

#[derive(Debug, Clone)]
pub struct Arg<'a> {
    pub id: &'a str,
    pub value: Inline<'a>,
}

mod selector_display {
    use core::fmt;

    use super::{NamedSelector, Selector, SelectorCombinator, SelectorFragment};

    impl fmt::Display for NamedSelector<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                NamedSelector::Tag(s) => write!(f, "{s}"),
                NamedSelector::Class(s) => write!(f, ".{s}"),
                NamedSelector::Id(s) => write!(f, "#{s}"),
            }
        }
    }

    impl fmt::Display for SelectorFragment<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                SelectorFragment::Any => f.write_str("*")?,
                SelectorFragment::Named(names) => {
                    // Technically unsound for a {tag}{tag} combo, but this is also
                    // not parsable currently.
                    for name in names.iter() {
                        write!(f, "{name}")?;
                    }
                }
            }

            Ok(())
        }
    }

    impl fmt::Display for SelectorCombinator {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                SelectorCombinator::Child => f.write_str(" > "),
                SelectorCombinator::Descendent => f.write_str(" "),
                SelectorCombinator::NextSibling => f.write_str(" + "),
                SelectorCombinator::SubsequentSibling => f.write_str(" ~ "),
            }
        }
    }

    impl fmt::Display for Selector<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.head)?;

            for (comb, frag) in self.combinators.iter() {
                write!(f, "{comb}{frag}")?;
            }

            Ok(())
        }
    }
}
