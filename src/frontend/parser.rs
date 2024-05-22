use core::fmt;
use std::borrow::Cow;

use super::{
    arena::Arena,
    ast::{
        ArgList, Ast, AstRef, Element, ElementList, ElementStatementList, FilterList, Leaf,
        Selector, SelectorCombinator, SelectorList, SelectorOpts, Statement,
    },
    scanner::{Lexeme, Scanner, Token},
};

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    arena: Arena<'a, Ast<'a>>,
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

type Result<'a, T> = std::result::Result<T, ParseError<'a>>;

impl<'a> Parser<'a> {
    #[must_use]
    pub const fn new(input: &'a str) -> Self {
        Self {
            scanner: Scanner::new(input),
            arena: Arena::new(),
        }
    }

    pub fn parse(
        mut self,
    ) -> Result<'a, (Arena<'a, Ast<'a>>, Option<AstRef<'a, ElementList<'a>>>)> {
        let r = self.parse_element_list()?;
        self.try_eat(Token::Eof)?;
        Ok((self.arena, r))
    }

    fn parse_element_list(&mut self) -> Result<'a, Option<AstRef<'a, ElementList<'a>>>> {
        let lx = self.scanner.peek_non_whitespace();

        match lx.token {
            Token::Eof => Ok(None),
            Token::Id | Token::Dot | Token::Hash | Token::Star => {
                let element = self.parse_element()?;
                let next = self.parse_element_list()?;
                let r = self.arena.insert_variant(ElementList::new(element, next));
                Ok(Some(r))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: vec![Token::Eof, Token::Id, Token::Dot, Token::Hash, Token::Star],
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
        let selector_head = self.parse_selector()?;
        let selectors = self.parse_selector_list()?;
        let lx = self.scanner.peek_non_whitespace();

        let ops = match lx.token {
            Token::Question => {
                self.scanner.eat_token();
                SelectorOpts::Optional
            }
            Token::Collection => {
                self.scanner.eat_token();
                SelectorOpts::Collection
            }
            _ => SelectorOpts::One,
        };

        self.try_eat(Token::BraceOpen)?;

        let statements = self.parse_element_statement_list()?;

        self.try_eat(Token::BraceClose)?;

        Ok(Element {
            selector_head,
            ops,
            selectors,
            statements,
        })
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
            Token::BraceOpen => return Ok(None),
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

    fn parse_element_statement_list(
        &mut self,
    ) -> Result<'a, Option<AstRef<'a, ElementStatementList<'a>>>> {
        let lx = self.scanner.peek_non_whitespace();
        let item = match lx.token {
            Token::At => Err(self.parse_statement()?),
            Token::Id | Token::Dot | Token::Hash | Token::Star => Ok(self.parse_element()?),
            Token::BraceClose | Token::Eof => return Ok(None),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: vec![
                        Token::At,
                        Token::Id,
                        Token::Dot,
                        Token::Hash,
                        Token::Star,
                        Token::BraceClose,
                        Token::Eof,
                    ],
                    got: lx,
                })
            }
        };

        let next = self.parse_element_statement_list()?;
        Ok(Some(
            self.arena
                .insert_variant(ElementStatementList::new(item, next)),
        ))
    }

    fn parse_statement(&mut self) -> Result<'a, Statement<'a>> {
        self.try_eat(Token::At)?;
        let id = self.try_eat(Token::Id)?.value;
        self.try_eat(Token::Colon)?;
        let value = self.try_eat(Token::Id)?.value;
        let filters = self.parse_filter_list()?;
        Ok(Statement { id, value, filters })
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
            let r = self.arena.insert_variant(FilterList::new(id, args, next));
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
                let lx = self.scanner.peek_non_whitespace();
                let value = match lx.token {
                    Token::Id => Leaf::Id(lx.value),
                    Token::Float => Leaf::Float(lx.value.parse().expect("float should be valid")),
                    Token::Int => Leaf::Int(lx.value.parse().expect("int should be valid")),
                    Token::String => Leaf::String(parse_string_literal(lx.value)),
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            expected: vec![Token::Id, Token::Float, Token::Int, Token::String],
                            got: lx,
                        })
                    }
                };
                self.scanner.eat_token();
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
                .filter_map(|(i, x)| prune.contains(&i).then_some(x))
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
            match &node.sel {
                SelectorCombinator::And(s) => write!(&mut out, "{s}"),
                SelectorCombinator::Child(s) => write!(&mut out, " > {s}"),
                SelectorCombinator::Descendent(s) => write!(&mut out, " {s}"),
                SelectorCombinator::NextSibling(s) => write!(&mut out, " + {s}"),
                SelectorCombinator::SubsequentSibling(s) => write!(&mut out, " ~ {s}"),
            }
            .expect("fmt error");
        }

        out
    }

    #[test]
    fn test_elements() {
        let string = r#"h1 {
            @x: me | cat(i: "x", ) | meow()
            h2#x > .cat  {
            }
            }"#;
        let parser = Parser::new(&string);
        let (arena, r) = parser.parse().expect("parsing failed");

        let elements = arena.flatten(r);
        let element = &elements[0].value;

        assert_eq!(
            fmt_selector(&element.selector_head, &arena.flatten(element.selectors)),
            "h1"
        );

        assert_eq!(element.ops, SelectorOpts::One);
        let statements = arena.flatten(element.statements);
        let stmt = statements[0]
            .value
            .as_ref()
            .expect_err("should be a statement");
        assert!(
            matches!(
                stmt,
                Statement {
                    id: "x",
                    value: "me",
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

        let element = statements[1].value.as_ref().expect("should b an element");
        assert!(element.statements.is_none());
        assert_eq!(
            fmt_selector(&element.selector_head, &arena.flatten(element.selectors)),
            "h2#x > .cat"
        );
    }
}
