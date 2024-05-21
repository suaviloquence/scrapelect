use core::fmt;

use super::{
    arena::{Arena, ArenaBuilder},
    ast::{
        Ast, AstRef, Element, ElementList, ElementStatementList, Selector, SelectorCombinator,
        SelectorList, SelectorOpts, Statement,
    },
    scanner::{Lexeme, Scanner, Token},
};

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    arena: ArenaBuilder<'a, Ast<'a>>,
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
            arena: Arena::builder(),
        }
    }

    pub fn parse(
        mut self,
    ) -> Result<'a, (Arena<'a, Ast<'a>>, Option<AstRef<'a, ElementList<'a>>>)> {
        let r = Self::parse_element_list(&mut self.scanner, &self.arena)?;
        Ok((self.arena.build(), r))
    }

    fn parse_element_list(
        scanner: &mut Scanner<'a>,
        arena: &ArenaBuilder<'a, Ast<'a>>,
    ) -> Result<'a, Option<AstRef<'a, ElementList<'a>>>> {
        let lx = scanner.peek_non_whitespace();

        match lx.token {
            Token::Eof => Ok(None),
            Token::Id | Token::Dot | Token::Hash | Token::Star => {
                let element = Self::parse_element(scanner, arena)?;
                let next = Self::parse_element_list(scanner, arena)?;
                let r = arena.insert_variant(ElementList::new(element, next));
                Ok(Some(r))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: vec![Token::Eof, Token::Id, Token::Dot, Token::Hash, Token::Star],
                got: lx,
            }),
        }
    }

    #[inline]
    fn try_eat(scanner: &mut Scanner<'a>, tk: Token) -> Result<'a, Lexeme<'a>> {
        let lx = scanner.peek_token();
        scanner.eat_token();

        if lx.token == tk {
            Ok(lx)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: vec![tk],
                got: lx,
            })
        }
    }

    fn parse_element(
        scanner: &mut Scanner<'a>,
        arena: &ArenaBuilder<'a, Ast<'a>>,
    ) -> Result<'a, Element<'a>> {
        let selector_head = Self::parse_selector(scanner, arena)?;
        let selectors = Self::parse_selector_list(scanner, arena)?;
        let lx = scanner.peek_non_whitespace();

        let ops = match lx.token {
            Token::Question => {
                scanner.eat_token();
                SelectorOpts::Optional
            }
            Token::Collection => {
                scanner.eat_token();
                SelectorOpts::Collection
            }
            _ => SelectorOpts::One,
        };

        Self::try_eat(scanner, Token::BraceOpen)?;

        let statements = Self::parse_element_statement_list(scanner, arena)?;

        Self::try_eat(scanner, Token::BraceClose)?;

        Ok(Element {
            selector_head,
            ops,
            selectors,
            statements,
        })
    }

    fn parse_selector_list(
        scanner: &mut Scanner<'a>,
        arena: &ArenaBuilder<'a, Ast<'a>>,
    ) -> Result<'a, Option<AstRef<'a, SelectorList<'a>>>> {
        let selector = Self::parse_selector(scanner, arena)?;
        let lx = scanner.peek_token();
        let sel = match lx.token {
            // slightly complicated: if it is whitespace and then another selector, it is
            // significant.  if it is whitespace then a brace, it is not significant.
            Token::Whitespace => {
                // if a brace comes after, we don't care about whitespace
                scanner.eat_token();
                if scanner.peek_non_whitespace().token == Token::BraceOpen {
                    return Ok(None);
                } else {
                    SelectorCombinator::Descendent(selector)
                }
            }
            Token::Plus => {
                scanner.eat_token();
                SelectorCombinator::NextSibling(selector)
            }
            Token::Tilde => {
                scanner.eat_token();
                SelectorCombinator::SubsequentSibling(selector)
            }
            Token::Hash | Token::Dot | Token::Id | Token::Star => SelectorCombinator::And(selector),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: vec![
                        Token::Whitespace,
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

        let itm = SelectorList::new(sel, Self::parse_selector_list(scanner, arena)?);

        Ok(Some(arena.insert_variant(itm)))
    }

    fn parse_selector(
        scanner: &mut Scanner<'a>,
        _: &ArenaBuilder<'a, Ast<'a>>,
    ) -> Result<'a, Selector<'a>> {
        let lx = scanner.peek_non_whitespace();
        match lx.token {
            Token::Dot => {
                scanner.eat_token();
                Self::try_eat(scanner, Token::Id).map(|lx| Selector::Class(lx.value))
            }
            Token::Hash => {
                scanner.eat_token();
                Self::try_eat(scanner, Token::Id).map(|lx| Selector::Id(lx.value))
            }
            Token::Id => {
                scanner.eat_token();
                Ok(Selector::Tag(lx.value))
            }
            Token::Star => {
                scanner.eat_token();
                Ok(Selector::Any)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: vec![Token::Dot, Token::Hash, Token::Id, Token::Star],
                got: lx,
            }),
        }
    }

    fn parse_element_statement_list(
        scanner: &mut Scanner<'a>,
        arena: &ArenaBuilder<'a, Ast<'a>>,
    ) -> Result<'a, Option<AstRef<'a, ElementStatementList<'a>>>> {
        let lx = scanner.peek_non_whitespace();
        let item = match lx.token {
            Token::At => Err(Self::parse_statement(scanner, arena)?),
            Token::Id | Token::Dot | Token::Hash | Token::Star => {
                Ok(Self::parse_element(scanner, arena)?)
            }
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

        let next = Self::parse_element_statement_list(scanner, arena)?;
        Ok(Some(
            arena.insert_variant(ElementStatementList::new(item, next)),
        ))
    }

    fn parse_statement(
        scanner: &mut Scanner<'a>,
        arena: &ArenaBuilder<'a, Ast<'a>>,
    ) -> Result<'a, Statement<'a>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;

    #[test]
    fn test_elements() {
        let string = "h1 {}";
        let parser = Parser::new(&string);
        panic!("{:?}", parser.parse());
    }
}
