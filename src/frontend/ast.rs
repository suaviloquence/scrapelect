use std::marker::PhantomData;

use super::arena::{Arena, Ref};

#[derive(Debug)]
pub struct AstRef<'a, T>(Ref<'a, Ast<'a>>, PhantomData<&'a T>);

pub trait AstType<'a>: Sized {
    #[must_use]
    fn unwrap_ref<'b>(node: &'b Ast<'a>) -> &'b Self;

    #[must_use]
    fn wrap(self) -> Ast<'a>;
}

/// explicit impl because we want it even when `T` is not Clone
impl<'a, T> Clone for AstRef<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<'a, T> Copy for AstRef<'a, T> {}

/// specialization for typed references
impl<'a> Arena<'a, Ast<'a>> {
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
}

#[derive(Debug, Clone)]
pub enum Selector<'a> {
    Any,
    Tag(&'a str),
    Class(&'a str),
    Id(&'a str),
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
                        $member: $ty,
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
    String(&'a str),
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
    SelectorList {
        sel: SelectorCombinator<'a>,
        next: Option<AstRef<'a, SelectorList<'a>>>,
    },
    ArgList {
        id: &'a str,
        value: Leaf<'a>,
        next: Option<AstRef<'a, ArgList<'a>>>,
    },
    FilterList {
        id: &'a str,
        args: Option<AstRef<'a, ArgList<'a>>>,
        next: Option<AstRef<'a, FilterList<'a>>>,
    },
    ElementStatementList {
        /// read as Either type
        value: Result<Element<'a>, Statement<'a>>,
        next: Option<AstRef<'a, ElementStatementList<'a>>>,
    },
    ElementList {
        value: Element<'a>,
        next: Option<AstRef<'a, ElementList<'a>>>,
    },
}
}
