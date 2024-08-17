use core::fmt;
use std::borrow::Cow;

use super::{
    ast::{
        Arg, Element, Filter, FilterCall, FilterSelect, FilterType, Inline, Leaf, NamedSelector,
        NonEmpty, Qualifier, RValue, Selector, SelectorCombinator, SelectorFragment, Statement,
    },
    scanner::{Lexeme, Scanner, Span, Token},
};

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum ParseError {
    UnexpectedToken {
        expected: Vec<Token>,
        got: Token,
        value: String,
        span: Span,
    },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken {
                expected,
                got,
                span,
                value,
            } => {
                write!(
                    f,
                    "Expected one of {expected:?}, got {got:?} '{value}' on line {}",
                    span.line
                )
            }
        }
    }
}

impl std::error::Error for ParseError {}

impl ParseError {
    /// Helper function to construct the `ParseError::UnexpectedToken` variant
    /// from a [`Lexeme`] and a [`Span`] and expected values.
    pub fn unexpected(expected: Vec<Token>, lx: Lexeme<'_>, span: Span) -> Self {
        Self::UnexpectedToken {
            expected,
            got: lx.token,
            value: lx.value.to_owned(),
            span,
        }
    }
}

type Result<T> = std::result::Result<T, ParseError>;

impl<'a> Parser<'a> {
    #[must_use]
    pub const fn new(input: &'a str) -> Self {
        Self {
            scanner: Scanner::new(input),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Statement<'a>>> {
        let statements = self.parse_statement_list()?;
        self.try_eat(Token::Eof)?;
        Ok(statements)
    }

    pub fn parse_statement_list(&mut self) -> Result<Vec<Statement<'a>>> {
        let mut vec = Vec::new();
        while let (
            _,
            Lexeme {
                token: Token::Id, ..
            },
        ) = self.scanner.peek_non_whitespace()
        {
            vec.push(self.parse_statement()?);
        }
        Ok(vec)
    }

    fn parse_statement(&mut self) -> Result<Statement<'a>> {
        let id = self.try_eat(Token::Id)?.value;
        self.try_eat(Token::Colon)?;
        let value = self.parse_rvalue()?;
        let filters = self.parse_filter_list()?;
        self.try_eat(Token::Semi)?;
        Ok(Statement { id, value, filters })
    }

    fn parse_rvalue(&mut self) -> Result<RValue<'a>> {
        let (_, lx) = self.scanner.peek_non_whitespace();

        match lx.token {
            Token::Id | Token::Less | Token::Dot | Token::Hash => {
                self.parse_element().map(RValue::Element)
            }
            _ => self.parse_leaf().map(RValue::Leaf),
        }
    }

    fn parse_leaf(&mut self) -> Result<Leaf<'a>> {
        self.scanner.peek_non_whitespace();
        let (span, lx) = self.scanner.eat_token();
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
            _ => Err(ParseError::unexpected(
                vec![Token::String, Token::Float, Token::Int, Token::Dollar],
                lx,
                span,
            )),
        }
    }

    #[inline]
    fn try_eat(&mut self, tk: Token) -> Result<Lexeme<'a>> {
        let (span, lx) = self.scanner.peek_non_whitespace();
        self.scanner.eat_token();

        if lx.token == tk {
            Ok(lx)
        } else {
            Err(ParseError::unexpected(vec![tk], lx, span))
        }
    }

    fn parse_element(&mut self) -> Result<Element<'a>> {
        let url = self.parse_maybe_url()?;
        let selector = self.parse_selector()?;

        self.try_eat(Token::BraceOpen)?;

        let statements = self.parse_statement_list()?;

        self.try_eat(Token::BraceClose)?;

        let qualifier = self.parse_qualifier()?;

        Ok(Element {
            url,
            selector,
            qualifier,
            statements,
        })
    }

    fn parse_maybe_url(&mut self) -> Result<Option<Inline<'a>>> {
        let (_, lx) = self.scanner.peek_non_whitespace();
        if lx.token == Token::Less {
            self.parse_inline().map(Some)
        } else {
            Ok(None)
        }
    }

    fn parse_inline(&mut self) -> Result<Inline<'a>> {
        self.try_eat(Token::Less)?;
        let value = self.parse_leaf()?;
        let filters = self.parse_filter_list()?;
        self.try_eat(Token::Greater)?;
        Ok(Inline { value, filters })
    }

    fn parse_value(&mut self) -> Result<Inline<'a>> {
        let (span, lx) = self.scanner.peek_non_whitespace();
        match lx.token {
            Token::Less => self.parse_inline(),
            Token::Dollar | Token::Int | Token::Float | Token::String => {
                self.parse_leaf().map(Inline::from)
            }
            _ => Err(ParseError::unexpected(
                vec![
                    Token::Less,
                    Token::Dollar,
                    Token::Int,
                    Token::Float,
                    Token::String,
                ],
                lx,
                span,
            )),
        }
    }

    fn parse_selector(&mut self) -> Result<Selector<'a>> {
        let head = self.parse_selector_fragment()?;

        let mut combinators = Vec::new();

        while let Some(combinator) = self.parse_selector_combinator()? {
            let fragment = self.parse_selector_fragment()?;

            combinators.push((combinator, fragment));
        }

        Ok(Selector { head, combinators })
    }

    fn parse_selector_combinator(&mut self) -> Result<Option<SelectorCombinator>> {
        let mut item = self.scanner.peek_non_comment();
        if item.1.token == Token::Whitespace {
            self.scanner.eat_token();
            let next = self.scanner.peek_non_whitespace();
            // if the next lexeme after the whitespace doesn't signify a selector,
            // the whitespace is not significant.
            match next.1.token {
                Token::Id | Token::Hash | Token::Dot | Token::Star => (),
                _ => item = next,
            };
        }

        let (span, lx) = item;

        match lx.token {
            Token::BraceOpen | Token::ParenOpen => Ok(None),
            // invariant: peek_next_whitespace is one of Id | Hash | Dot | Star
            // whitespace is eaten in the above block.
            Token::Whitespace => Ok(Some(SelectorCombinator::Descendent)),
            Token::Greater => {
                self.scanner.eat_token();
                Ok(Some(SelectorCombinator::Child))
            }
            Token::Plus => {
                self.scanner.eat_token();
                Ok(Some(SelectorCombinator::NextSibling))
            }
            Token::Tilde => {
                self.scanner.eat_token();
                Ok(Some(SelectorCombinator::SubsequentSibling))
            }
            _ => Err(ParseError::unexpected(
                vec![Token::Whitespace, Token::Greater, Token::Plus, Token::Tilde],
                lx,
                span,
            )),
        }
    }

    fn parse_selector_fragment(&mut self) -> Result<SelectorFragment<'a>> {
        let (span, lx) = self.scanner.peek_non_whitespace();

        match lx.token {
            Token::Star => Ok(SelectorFragment::Any),
            Token::Dot | Token::Id | Token::Hash => {
                let named_selector = self.parse_named_selector()?;
                let mut selector = NonEmpty::from_one(named_selector);

                while let Token::Dot | Token::Id | Token::Hash =
                    self.scanner.peek_non_whitespace().1.token
                {
                    selector.push(self.parse_named_selector()?);
                }

                Ok(SelectorFragment::Named(selector))
            }
            _ => Err(ParseError::unexpected(
                vec![Token::Star, Token::Dot, Token::Id, Token::Hash],
                lx,
                span,
            )),
        }
    }

    fn parse_named_selector(&mut self) -> Result<NamedSelector<'a>> {
        let (span, lx) = self.scanner.peek_non_whitespace();
        match lx.token {
            Token::Dot => {
                self.scanner.eat_token();
                self.try_eat(Token::Id)
                    .map(|lx| NamedSelector::Class(lx.value))
            }
            Token::Hash => {
                self.scanner.eat_token();
                self.try_eat(Token::Id)
                    .map(|lx| NamedSelector::Id(lx.value))
            }
            Token::Id => {
                self.scanner.eat_token();
                Ok(NamedSelector::Tag(lx.value))
            }
            _ => Err(ParseError::unexpected(
                vec![Token::Dot, Token::Hash, Token::Id],
                lx,
                span,
            )),
        }
    }

    fn parse_filter_list(&mut self) -> Result<Vec<Filter<'a>>> {
        let mut vec = Vec::new();

        while let Token::Pipe = self.scanner.peek_non_whitespace().1.token {
            self.scanner.eat_token();
            let filter = self.parse_filter()?;
            let qualifier = self.parse_qualifier()?;
            vec.push(Filter { filter, qualifier })
        }

        Ok(vec)
    }

    fn parse_filter(&mut self) -> Result<FilterType<'a>> {
        let (span, lx) = self.scanner.peek_non_whitespace();
        self.scanner.eat_token();

        match lx.token {
            Token::Id => {
                let id = lx.value;
                self.try_eat(Token::ParenOpen)?;
                let args = self.parse_arg_list()?;
                self.try_eat(Token::ParenClose)?;
                Ok(FilterType::Call(FilterCall { id, args }))
            }
            Token::BracketOpen => {
                let name = self.try_eat(Token::Id)?.value;
                self.try_eat(Token::Colon)?;
                let leaf = self.parse_leaf()?;
                let filters = self.parse_filter_list()?;
                self.try_eat(Token::BracketClose)?;
                Ok(FilterType::Select(FilterSelect {
                    name,
                    value: Inline {
                        value: leaf,
                        filters,
                    },
                }))
            }
            _ => Err(ParseError::unexpected(
                vec![Token::Id, Token::BracketOpen],
                lx,
                span,
            )),
        }
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Arg<'a>>> {
        let mut vec = Vec::new();
        loop {
            let (span, lx) = self.scanner.peek_non_whitespace();
            match lx.token {
                Token::ParenClose => break,
                Token::Id => {
                    let id = lx.value;
                    self.scanner.eat_token();
                    self.try_eat(Token::Colon)?;
                    let value = self.parse_value()?;

                    vec.push(Arg { id, value });

                    // eat trailing comma.  if there is no comma, there can be
                    // no subsequent args.
                    if let Token::Comma = self.scanner.peek_non_whitespace().1.token {
                        self.scanner.eat_token();
                    } else {
                        break;
                    }
                }
                _ => {
                    return Err(ParseError::unexpected(
                        vec![Token::ParenClose, Token::Id],
                        lx,
                        span,
                    ))
                }
            }
        }

        Ok(vec)
    }

    fn parse_qualifier(&mut self) -> Result<Qualifier> {
        let (_, lx) = self.scanner.peek_non_whitespace();
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
    let mut replace = vec![];
    let s = &s[1..s.len() - 1];

    let mut escape_next = false;
    for (i, s) in s.char_indices() {
        if escape_next {
            escape_next = false;
            let escaped = match s {
                'n' => '\n',
                '\\' => '\\',
                '"' => '"',
                other => {
                    // TODO
                    eprintln!("Unknown escape character {other:?}");
                    other
                }
            };

            replace.push((i, Some(escaped)));
        } else if s == '\\' {
            escape_next = true;
            replace.push((i, None));
        }
    }

    if replace.is_empty() {
        Cow::Borrowed(s)
    } else {
        let mut replace = replace.into_iter().peekable();
        Cow::Owned(
            s.char_indices()
                .filter_map(|(i, x)| {
                    replace
                        .peek()
                        .copied()
                        .and_then(|(j, v)| {
                            if i == j {
                                let _ = replace.next();
                                Some(v)
                            } else {
                                None
                            }
                        })
                        .unwrap_or(Some(x))
                })
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::{parse_string_literal, Parser};
    use crate::frontend::ast::*;

    #[test]
    fn test_parse() {
        let string = r#"a: h1 {
                x: $me | cat(i: "x", ) | meow();

                y: h2#x > .cat  {

                };
            };"#;
        let parser = Parser::new(string);
        let stmts = parser.parse().expect("parsing failed");

        let stmt = &stmts[0];

        assert_eq!(stmt.id, "a");
        let RValue::Element(element) = &stmt.value else {
            panic!("expected element");
        };

        assert_eq!(element.selector.to_string(), "h1");

        assert_eq!(element.qualifier, Qualifier::One);

        let stmt = &element.statements[0];

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

        let filters = &stmt.filters;
        assert!(
            matches!(
                &filters[..],
                [
                    Filter {
                        filter: FilterType::Call(FilterCall { id: "cat", .. }),
                        ..
                    },
                    Filter {
                        filter: FilterType::Call(FilterCall { id: "meow", .. }),
                        ..
                    }
                ]
            ),
            "found {filters:?}"
        );

        let FilterType::Call(filter) = &filters[0].filter else {
            unreachable!("Validated as Filter::Call above");
        };
        let args = &filter.args;
        assert!(
            matches!(
                &args[..],
                [Arg {
                    id: "i",
                    value: Inline {
                        value: Leaf::String(Cow::Borrowed("x")),
                        filters: _,
                    },
                    ..
                }]
            ),
            "found {:?}",
            &args[..]
        );

        let stmt = &element.statements[1];

        let RValue::Element(element) = &stmt.value else {
            panic!("Expected element");
        };

        assert!(element.statements.is_empty());
        assert_eq!(element.selector.to_string(), "h2#x > .cat");
    }

    #[test]
    fn test_escape_strings() {
        assert_eq!(parse_string_literal(r#""""#), "");
        assert_eq!(parse_string_literal(r#""abcdef""#), "abcdef");
        assert_eq!(parse_string_literal(r#""hello! \n""#), "hello! \n");
        assert_eq!(
            parse_string_literal(r#""my \" crazy \\ lifestyle \\\"""#),
            r#"my " crazy \ lifestyle \""#
        );
    }
}
