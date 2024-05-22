use core::{fmt, marker::PhantomData};
use std::borrow::Cow;

use super::arena::{Arena, Ref};

pub type AstArena<'a> = Arena<'a, Ast<'a>>;

#[derive(Debug)]
pub struct AstRef<'a, T>(Ref<'a, Ast<'a>>, PhantomData<&'a T>);

pub trait AstType<'a>: Sized {
    #[must_use]
    fn unwrap_ref<'b>(node: &'b Ast<'a>) -> &'b Self;

    #[must_use]
    fn wrap(self) -> Ast<'a>;
}

pub trait AstArenaFlatten<'a>: AstType<'a> {
    fn flatten<'s: 'o, 'o>(&'s self, arena: &'s AstArena<'a>, out: &mut Vec<&'o Self>);
}

/// explicit impl because we want it even when `T` is not Clone
impl<'a, T> Clone for AstRef<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<'a, T> Copy for AstRef<'a, T> {}

/// specialization for typed references
impl<'a> AstArena<'a> {
    #[inline]
    #[must_use]
    pub fn get_variant<'s, T>(&'s self, r: AstRef<'a, T>) -> &'s T
    where
        'a: 's,
        T: AstType<'a>,
    {
        T::unwrap_ref(self.get(r.0))
    }

    pub fn insert_variant<'s, T: AstType<'a>>(&'s mut self, variant: T) -> AstRef<'a, T>
    where
        'a: 's,
    {
        let r = self.insert(variant.wrap());
        AstRef(r, PhantomData)
    }

    #[must_use]
    pub fn flatten<T: AstArenaFlatten<'a>>(&self, r: Option<AstRef<'a, T>>) -> Vec<&T> {
        let mut out = Vec::new();
        if let Some(r) = r {
            self.get_variant(r).flatten(&self, &mut out);
        }
        out
    }
}

#[derive(Debug, Clone)]
pub enum Selector<'a> {
    Any,
    Tag(&'a str),
    Class(&'a str),
    Id(&'a str),
}

impl fmt::Display for Selector<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => write!(f, "*"),
            Self::Tag(id) => write!(f, "{id}"),
            Self::Class(id) => write!(f, ".{id}"),
            Self::Id(id) => write!(f, "#{id}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SelectorCombinator<'a> {
    NextSibling(Selector<'a>),
    Child(Selector<'a>),
    SubsequentSibling(Selector<'a>),
    Descendent(Selector<'a>),
    And(Selector<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelectorOpts {
    One,
    Optional,
    Collection,
}

macro_rules! ast_enum {
        {
            ALL:
            // macro limitations can only have nonrepeating metavariable  at this scope
            // see https://github.com/rust-lang/rust/issues/96184
            #[$allmeta: meta]
            AST:
            $(#[$astmeta: meta])*
            pub enum Ast<'a> {
                $(
                    $(#[$indivmeta: meta])*
                    $(@flatten[$(.$preorder: ident, )* self $(, .$postorder: ident)*])?
                    $variant: ident {
                        $(
                            $(#[$membermeta: meta])*
                            $member: ident: $ty: ty,
                        )*
                    },
                )*
            }
        } => {
            $(
                #[$allmeta]
                #[non_exhaustive]
                $(#[$indivmeta])*
                pub struct $variant<'a> {
                    $(
                        $(#[$membermeta])*
                        pub $member: $ty,
                    )*
                    lt: PhantomData<&'a ()>,
                }

                impl<'a> $variant<'a> {
                    #[must_use]
                    #[inline]
                    pub const fn new($($member: $ty, )*) -> Self {
                        Self { lt: PhantomData, $($member, )* }
                    }
                }

                impl<'a> AstType<'a> for $variant<'a> {
                    #[inline]
                    #[must_use]
                    fn unwrap_ref<'b>(node: &'b Ast<'a>) -> &'b Self {
                        let Ast::$variant(x) = node else { unreachable!(concat!("expected a `Ast::", stringify!($variant), "` variant."))};
                        x
                    }

                    #[inline]
                    #[must_use]
                    fn wrap(self) -> Ast<'a> {
                        Ast::$variant(self)
                    }
                }

                $(
                    impl<'a> AstArenaFlatten<'a> for $variant<'a> {
                        fn flatten<'s: 'o, 'o>(&'s self, arena: &'s AstArena<'a>, out: &mut Vec<&'o Self>) {
                            $(
                                if let Some(pre) = self.$preorder {
                                    arena.get_variant(pre).flatten(arena, out);
                                }
                            )*
                            out.push(self);
                            $(
                                if let Some(post) = self.$postorder {
                                    arena.get_variant(post).flatten(arena, out);
                                }
                            )*
                        }
                    }
                )?
            )*

            #[$allmeta]
            $(#[$astmeta])*
            pub enum Ast<'a> {
                $($variant($variant<'a>),)*
            }
        };
    }

#[derive(Debug, Clone)]
pub enum Leaf<'a> {
    Id(&'a str),
    Int(i64),
    Float(f64),
    String(Cow<'a, str>),
}

#[derive(Debug, Clone)]
pub struct Statement<'a> {
    pub id: &'a str,
    pub value: &'a str,
    pub filters: Option<AstRef<'a, FilterList<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Element<'a> {
    pub selector_head: Selector<'a>,
    pub selectors: Option<AstRef<'a, SelectorList<'a>>>,
    pub ops: SelectorOpts,
    pub statements: Option<AstRef<'a, ElementStatementList<'a>>>,
}

ast_enum! {
ALL:
#[derive(Debug, Clone)]
AST:
/// An abstract syntax tree
pub enum Ast<'a> {
    @flatten[self, .next]
    SelectorList {
        sel: SelectorCombinator<'a>,
        next: Option<AstRef<'a, SelectorList<'a>>>,
    },
    @flatten[self, .next]
    ArgList {
        id: &'a str,
        value: Leaf<'a>,
        next: Option<AstRef<'a, ArgList<'a>>>,
    },
    @flatten[self, .next]
    FilterList {
        id: &'a str,
        args: Option<AstRef<'a, ArgList<'a>>>,
        next: Option<AstRef<'a, FilterList<'a>>>,
    },
    @flatten[self, .next]
    ElementStatementList {
        /// read as Either type
        value: Result<Element<'a>, Statement<'a>>,
        next: Option<AstRef<'a, ElementStatementList<'a>>>,
    },
    @flatten[self, .next]
    ElementList {
        value: Element<'a>,
        next: Option<AstRef<'a, ElementList<'a>>>,
    },
}
}
