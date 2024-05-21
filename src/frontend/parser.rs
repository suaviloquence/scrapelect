use core::fmt;

use super::{
    arena::Arena,
    ast::{
        Ast, AstRef, Element, ElementList, ElementStatementList, Selector, SelectorCombinator,
        SelectorList, SelectorOpts, Statement,
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
        let lx = self.scanner.peek_token();
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
        let selector = self.parse_selector()?;
        let lx = self.scanner.peek_token();
        let sel = match lx.token {
            // slightly complicated: if it is whitespace and then another selector, it is
            // significant.  if it is whitespace then a brace, it is not significant.
            Token::Whitespace => {
                // if a brace comes after, we don't care about whitespace
                self.scanner.eat_token();
                if self.scanner.peek_non_whitespace().token == Token::BraceOpen {
                    return Ok(None);
                } else {
                    SelectorCombinator::Descendent(selector)
                }
            }
            Token::Plus => {
                self.scanner.eat_token();
                SelectorCombinator::NextSibling(selector)
            }
            Token::Tilde => {
                self.scanner.eat_token();
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
