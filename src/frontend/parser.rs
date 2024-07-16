use core::fmt;
use std::{borrow::Cow, ops::Not};

use super::{
    arena::Arena,
    ast::{
        ArgList, Ast, AstRef, Element, FilterList, Inline, Leaf, Qualifier, RValue, Selector,
        SelectorCombinator, SelectorList, Statement, StatementList,
    },
    scanner::{Lexeme, Scanner, Token},
};

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    arena: Arena<Ast<'a>>,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum ParseError<'a> {
    UnexpectedToken {
        expected: Vec<Token>,
        got: Lexeme<'a>,
    },
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken { expected, got } => {
                write!(f, "Expected one of {expected:?}, got {got:?}")
            }
        }
    }
}

impl std::error::Error for ParseError<'_> {}

type Result<'a, T> = std::result::Result<T, ParseError<'a>>;

impl<'a> Parser<'a> {
    #[must_use]
    pub const fn new(input: &'a str) -> Self {
        Self {
            scanner: Scanner::new(input),
            arena: Arena::new(),
        }
    }

    pub fn parse(mut self) -> Result<'a, (Arena<Ast<'a>>, Option<AstRef<'a, StatementList<'a>>>)> {
        let r = match self.parse_statement_list() {
            Ok(r) => r,
            Err(e) => {
                return Err(e);
            }
        };
        self.try_eat(Token::Eof)?;
        Ok((self.arena, r))
    }

    pub fn parse_statement_list(&mut self) -> Result<'a, Option<AstRef<'a, StatementList<'a>>>> {
        let lx = self.scanner.peek_non_whitespace();

        if lx.token == Token::Id {
            let statement = self.parse_statement()?;
            let next = self.parse_statement_list()?;

            Ok(Some(
                self.arena
                    .insert_variant(StatementList::new(statement, next)),
            ))
        } else {
            Ok(None)
        }
    }

    fn parse_statement(&mut self) -> Result<'a, Statement<'a>> {
        let id = self.try_eat(Token::Id)?.value;
        self.try_eat(Token::Colon)?;
        let value = self.parse_rvalue()?;
        let filters = self.parse_filter_list()?;
        self.try_eat(Token::Semi)?;
        Ok(Statement { id, value, filters })
    }

    fn parse_rvalue(&mut self) -> Result<'a, RValue<'a>> {
        let lx = self.scanner.peek_non_whitespace();

        match lx.token {
            Token::Id | Token::Less | Token::Dot | Token::Hash => {
                self.parse_element().map(RValue::Element)
            }
            _ => self.parse_leaf().map(RValue::Leaf),
        }
    }

    fn parse_leaf(&mut self) -> Result<'a, Leaf<'a>> {
        self.scanner.peek_non_whitespace();
        let lx = self.scanner.eat_token();
        match lx.token {
            Token::String => Ok(Leaf::String(parse_string_literal(lx.value))),
            Token::Float => Ok(Leaf::Float(
                lx.value.parse().expect("float literal invalid"),
            )),
            Token::Int => Ok(Leaf::Int(lx.value.parse().expect("int literal invalid"))),
            Token::Dollar => {
                let id = self.try_eat(Token::Id)?.value;
                Ok(Leaf::Var(id))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: vec![Token::String, Token::Float, Token::Int, Token::Dollar],
                got: lx,
            }),
        }
    }

    #[inline]
    fn try_eat(&mut self, tk: Token) -> Result<'a, Lexeme<'a>> {
        let lx = self.scanner.peek_non_whitespace();
        self.scanner.eat_token();

        if lx.token == tk {
            Ok(lx)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: vec![tk],
                got: lx,
            })
        }
    }

    fn parse_element(&mut self) -> Result<'a, Element<'a>> {
        let url = self.parse_maybe_url()?;
        let selector_head = self.parse_selector()?;
        let selectors = self.parse_selector_list()?;

        self.try_eat(Token::BraceOpen)?;

        let statements = self.parse_statement_list()?;

        self.try_eat(Token::BraceClose)?;

        let qualifier = self.parse_qualifier()?;

        Ok(Element {
            url,
            selector_head,
            selectors,
            qualifier,
            statements,
        })
    }

    fn parse_maybe_url(&mut self) -> Result<'a, Option<Inline<'a>>> {
        let lx = self.scanner.peek_non_whitespace();
        if lx.token == Token::Less {
            self.parse_inline().map(Some)
        } else {
            Ok(None)
        }
    }

    fn parse_inline(&mut self) -> Result<'a, Inline<'a>> {
        let lx = self.scanner.peek_non_whitespace();
        if lx.token == Token::Less {
            self.scanner.eat_token();
            let value = self.parse_leaf()?;
            let filters = self.parse_filter_list()?;
            self.try_eat(Token::Greater)?;
            Ok(Inline { value, filters })
        } else {
            Err(ParseError::UnexpectedToken {
                expected: vec![Token::Less],
                got: lx,
            })
        }
    }

    fn parse_selector_list(&mut self) -> Result<'a, Option<AstRef<'a, SelectorList<'a>>>> {
        let mut lx = self.scanner.peek_token();
        if lx.token == Token::Whitespace {
            self.scanner.eat_token();
            let next_lx = self.scanner.peek_non_whitespace();
            // if the next lexeme after the whitespace doesn't signify a selector,
            // the whitespace is not significant.
            match next_lx.token {
                Token::Id | Token::Hash | Token::Dot | Token::Star => (),
                _ => lx = next_lx,
            };
        }

        let sel = match lx.token {
            Token::BraceOpen | Token::ParenOpen => return Ok(None),
            // invariant: peek_next_whitespace is one of Id | Hash | Dot | Star
            // whitespace is eaten in the above block.
            Token::Whitespace => SelectorCombinator::Descendent(self.parse_selector()?),
            Token::Greater => {
                self.scanner.eat_token();
                SelectorCombinator::Child(self.parse_selector()?)
            }
            Token::Plus => {
                self.scanner.eat_token();
                SelectorCombinator::NextSibling(self.parse_selector()?)
            }
            Token::Tilde => {
                self.scanner.eat_token();
                SelectorCombinator::SubsequentSibling(self.parse_selector()?)
            }
            Token::Hash | Token::Dot | Token::Id | Token::Star => {
                SelectorCombinator::And(self.parse_selector()?)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: vec![
                        Token::Whitespace,
                        Token::Greater,
                        Token::Plus,
                        Token::Tilde,
                        Token::Hash,
                        Token::Dot,
                        Token::Id,
                        Token::Star,
                    ],
                    got: lx,
                })
            }
        };

        let itm = SelectorList::new(sel, self.parse_selector_list()?);

        Ok(Some(self.arena.insert_variant(itm)))
    }

    fn parse_selector(&mut self) -> Result<'a, Selector<'a>> {
        let lx = self.scanner.peek_non_whitespace();
        match lx.token {
            Token::Dot => {
                self.scanner.eat_token();
                self.try_eat(Token::Id).map(|lx| Selector::Class(lx.value))
            }
            Token::Hash => {
                self.scanner.eat_token();
                self.try_eat(Token::Id).map(|lx| Selector::Id(lx.value))
            }
            Token::Id => {
                self.scanner.eat_token();
                Ok(Selector::Tag(lx.value))
            }
            Token::Star => {
                self.scanner.eat_token();
                Ok(Selector::Any)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: vec![Token::Dot, Token::Hash, Token::Id, Token::Star],
                got: lx,
            }),
        }
    }

    fn parse_filter_list(&mut self) -> Result<'a, Option<AstRef<'a, FilterList<'a>>>> {
        let lx = self.scanner.peek_non_whitespace();
        if lx.token == Token::Pipe {
            self.scanner.eat_token();
            let id = self.try_eat(Token::Id)?.value;
            self.try_eat(Token::ParenOpen)?;
            let args = self.parse_arg_list()?;
            self.try_eat(Token::ParenClose)?;
            let next = self.parse_filter_list()?;
            let qualifier = self.parse_qualifier()?;
            let r = self
                .arena
                .insert_variant(FilterList::new(id, args, next, qualifier));
            Ok(Some(r))
        } else {
            Ok(None)
        }
    }

    fn parse_arg_list(&mut self) -> Result<'a, Option<AstRef<'a, ArgList<'a>>>> {
        let lx = self.scanner.peek_non_whitespace();
        match lx.token {
            Token::ParenClose => Ok(None),
            Token::Id => {
                let id = lx.value;
                self.scanner.eat_token();
                self.try_eat(Token::Colon)?;
                let value = self.parse_leaf()?;
                let next = match self.scanner.peek_non_whitespace().token {
                    Token::Comma => {
                        self.scanner.eat_token();
                        self.parse_arg_list()?
                    }
                    _ => None,
                };

                let r = self.arena.insert_variant(ArgList::new(id, value, next));
                Ok(Some(r))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: vec![Token::ParenClose, Token::Id],
                got: lx,
            }),
        }
    }

    fn parse_qualifier(&mut self) -> Result<'a, Qualifier> {
        let lx = self.scanner.peek_non_whitespace();
        Ok(match lx.token {
            Token::Question => {
                self.scanner.eat_token();
                Qualifier::Optional
            }
            Token::Star => {
                self.scanner.eat_token();
                Qualifier::Collection
            }
            _ => Qualifier::One,
        })
    }
}

fn parse_string_literal(s: &str) -> Cow<'_, str> {
    debug_assert!(s.len() >= 2 && &s[0..1] == "\"" && &s[s.len() - 1..] == "\"");
    let mut prune = vec![];
    let s = &s[1..s.len() - 1];

    let mut escape_next = false;
    for (i, s) in s.char_indices() {
        if escape_next {
            escape_next = false;
        } else if s == '\\' {
            escape_next = true;
            prune.push(i);
        }
    }

    if prune.is_empty() {
        Cow::Borrowed(s)
    } else {
        Cow::Owned(
            s.char_indices()
                .filter_map(|(i, x)| prune.contains(&i).not().then_some(x))
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::Parser;
    use crate::frontend::ast::*;

    fn fmt_selector<'a>(head: &Selector<'a>, list: &[&SelectorList<'a>]) -> String {
        use std::fmt::Write as _;
        let mut out = String::new();
        write!(&mut out, "{head}").expect("fmt error");
        for node in list {
            let _ = match &node.sel {
                SelectorCombinator::And(s) => write!(&mut out, "{s}"),
                SelectorCombinator::Child(s) => write!(&mut out, " > {s}"),
                SelectorCombinator::Descendent(s) => write!(&mut out, " {s}"),
                SelectorCombinator::NextSibling(s) => write!(&mut out, " + {s}"),
                SelectorCombinator::SubsequentSibling(s) => write!(&mut out, " ~ {s}"),
            };
        }

        out
    }

    #[test]
    fn test_parse() {
        let string = r#"a: h1 {
                x: $me | cat(i: "x", ) | meow();

                y: h2#x > .cat  {

                };
            };"#;
        let parser = Parser::new(&string);
        let (arena, r) = parser.parse().expect("parsing failed");

        let stmts = arena.flatten(r);
        let stmt = &stmts[0].value;

        assert_eq!(stmt.id, "a");
        let RValue::Element(element) = &stmt.value else {
            panic!("expected element");
        };

        assert_eq!(
            fmt_selector(&element.selector_head, &arena.flatten(element.selectors)),
            "h1"
        );

        assert_eq!(element.qualifier, Qualifier::One);
        let statements = arena.flatten(element.statements);

        let stmt = &statements[0].value;

        assert!(
            matches!(
                stmt,
                Statement {
                    id: "x",
                    value: RValue::Leaf(Leaf::Var("me")),
                    ..
                }
            ),
            "found {stmt:?}",
        );

        let filters = arena.flatten(stmt.filters);
        assert!(
            matches!(
                &filters[..],
                [FilterList { id: "cat", .. }, FilterList { id: "meow", .. }]
            ),
            "found {filters:?}"
        );

        let filter = filters[0];
        let args = arena.flatten(filter.args);
        assert!(
            matches!(
                &args[..],
                [ArgList {
                    id: "i",
                    value: Leaf::String(Cow::Borrowed("x")),
                    ..
                }]
            ),
            "found {:?}",
            &args[..]
        );

        let stmt = &statements[1].value;

        let RValue::Element(element) = &stmt.value else {
            panic!("Expected element");
        };

        assert!(element.statements.is_none());
        assert_eq!(
            fmt_selector(&element.selector_head, &arena.flatten(element.selectors)),
            "h2#x > .cat"
        );
    }
}
