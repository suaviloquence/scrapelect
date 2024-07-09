#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    /// an identifier
    Id,
    /// a literal integer
    Int,
    /// a literal decimal number
    Float,
    /// a literal string
    String,
    /// the selector combinator `.` to indicate a class name
    Dot,
    /// the selector combinator `#` to indicate an id name
    Hash,
    /// the selector combinator `*` to match all elements
    Star,
    /// the selector combinator `+` to indicate the next sibling
    Plus,
    /// the selector combinator `>` to indicate a direct child
    Greater,
    /// the selector combinator `~` to indicate a subsequent sibling
    Tilde,
    /// any whitespace, significant to separate e.g., `a#id` from  `a #id`
    Whitespace,
    /// the selector option `?` to indicate zero or one item
    Question,
    /// the selector option `[]` to indicate a collection of items
    Collection,
    /// an opening brace `{` to start an element block
    BraceOpen,
    /// a closing brace `}` to end an element block
    BraceClose,
    /// a dollar sign `$` to dereference a variable
    Dollar,
    /// a pipe `|` to indicate a filter
    Pipe,
    /// an opening parenthesis `(` to start a filter call
    ParenOpen,
    /// a closing parenthesis `)` to end a filter call
    ParenClose,
    /// a comma `,` to separate arguments in a list, or to indicate a different selector
    Comma,
    /// a colon `:` to separate id from value in statements and arguments
    Colon,
    /// a semicolon `;` to indicate the end of a statement
    Semi,
    /// special token to indicate the end of the file
    Eof,
    /// special token to indicate unknown token
    Unknown,
}

mod statics {
    use super::Token;
    use regex::{Regex, RegexSet};
    use std::sync::OnceLock;

    pub struct Lazy<T, F = fn() -> T>(OnceLock<T>, F);

    impl<T, F: Fn() -> T> Lazy<T, F> {
        pub const fn new(f: F) -> Self {
            Self(OnceLock::new(), f)
        }
    }

    impl<T, F: Fn() -> T> std::ops::Deref for Lazy<T, F> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            self.0.get_or_init(&self.1)
        }
    }

    macro_rules! make_regex_set {
        {$vis: vis ($tokens: ident, $re_set: ident, $re_compiled: ident) = {$($tk: ident <- $pat: literal)*};} => {
            $vis static $tokens: &[Token] = &[
                $(Token::$tk, )*
            ];

            $vis static $re_set: Lazy<RegexSet> = Lazy::new(|| RegexSet::new(&[
                $(
                    concat!("^", $pat),
                )*
                ]).expect("error building RegexSet"));

            $vis static $re_compiled: Lazy<Vec<Regex>> = Lazy::new(|| vec![
                $(
                    Regex::new(concat!("^", $pat)).expect(concat!("Error building Regex `", $pat, "`")),
                )*
            ]);
        };
    }

    make_regex_set! {
        pub(super) (TOKENS, REGEX_SET, REGEX_LIST) = {
            Id <- "[a-zA-Z][a-zA-Z0-9_-]*"
            Int <- "[+-]?[0-9]+"
            // must match at least one number before decimal pt, but not necessarily after
            Float <- r"[+-]?[0-9]+\.[0-9]*"
            String <- r#""(\\.|[^\\"])*""#
            Dot <- r"\."
            Hash <- "#"
            Star <- r"\*"
            Plus <- r"\+"
            Greater <- ">"
            Tilde <- "~"
            Whitespace <- r"\p{White_Space}+"
            Question <- r"\?"
            Collection <- r"\[\]"
            BraceOpen <- r"\{"
            BraceClose <- r"\}"
            Dollar <- r"\$"
            Pipe <- r"\|"
            ParenOpen <- r"\("
            ParenClose <- r"\)"
            Comma <- ","
            Colon <- ":"
            Semi <- ";"
        };
    }
}

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    slice: &'a str,
    idx: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lexeme<'a> {
    pub token: Token,
    pub value: &'a str,
}

const EOF: Lexeme = Lexeme {
    token: Token::Eof,
    value: "",
};

impl<'a> Scanner<'a> {
    #[must_use]
    pub const fn new(slice: &'a str) -> Self {
        Self { slice, idx: 0 }
    }

    #[must_use]
    pub fn peek_token(&self) -> Lexeme<'a> {
        if self.idx >= self.slice.len() {
            return EOF;
        }

        // note to self: we can't use find_at because it still considers the
        // start of the string (e.g., for `^`) to be i = 0, not i = idx.
        statics::REGEX_SET
            .matches(&self.slice[self.idx..])
            .into_iter()
            .map(|x| Lexeme {
                token: statics::TOKENS[x],
                value: statics::REGEX_LIST[x]
                    .find(&self.slice[self.idx..])
                    .expect("matched in set should match in list")
                    .as_str(),
            })
            .max_by_key(|x| x.value.len())
            .unwrap_or(Lexeme {
                token: Token::Unknown,
                value: &self.slice[self.idx..self.idx + 1],
            })
    }

    pub fn eat_token(&mut self) -> Lexeme<'a> {
        let lexeme = self.peek_token();
        self.idx += lexeme.value.len();
        lexeme
    }

    pub fn peek_non_whitespace(&mut self) -> Lexeme<'a> {
        while let Lexeme {
            token: Token::Whitespace,
            ..
        } = self.peek_token()
        {
            self.eat_token();
        }
        self.peek_token()
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexeme, Scanner, Token, EOF};

    #[test]
    fn test_tokens() {
        let scanner = Scanner::new("");
        assert_eq!(scanner.peek_token(), EOF);

        macro_rules! test_matches {
            {$($tk: ident => $($pat: literal)+ $(!($($npat: literal)+))?)* } => {
                $(
                    $(
                        assert_eq!(
                            Scanner::new($pat).peek_token(),
                            Lexeme { token: Token::$tk, value: $pat }
                        );
                    )+

                    $(
                        $(
                            assert_ne!(
                                Scanner::new($npat).peek_token(),
                                Lexeme { token: Token::$tk, value: $npat }
                            );
                        )*
                    )?
                )*
            };
        }

        test_matches! {
            Id => "a" "a-" "A9-9-9-9" "a____a" !("9" "-" "_")
            Int => "+1" "1" "1234" "-1" !("+" "-")
            Float => "0." "-0.1234" "+0.12345" !("1" ".5" "-.5" ".")
            String => r#""hello!""# r#""""# r#""\"""# !(r#"""""# r#""\""#)
            // ensure no regressions because we have to escape these
            Dot => "." !("a")
            Star => "*"
            Plus => "+"
            Question => "?"
            Pipe => "|"
        }
    }

    macro_rules! lx {
        ($tk: ident, $lit: literal) => {
            Lexeme {
                token: Token::$tk,
                value: $lit,
            }
        };
    }

    #[test]
    fn test_eat() {
        let mut sc = Scanner::new("h3 h4#h5.h6 {}");
        assert_eq!(sc.eat_token(), lx!(Id, "h3"));
        assert_eq!(sc.eat_token(), lx!(Whitespace, " "));
        assert_eq!(sc.eat_token(), lx!(Id, "h4"));
        assert_eq!(sc.eat_token(), lx!(Hash, "#"));
        assert_eq!(sc.eat_token(), lx!(Id, "h5"));
        assert_eq!(sc.eat_token(), lx!(Dot, "."));
        assert_eq!(sc.eat_token(), lx!(Id, "h6"));
        assert_eq!(sc.eat_token(), lx!(Whitespace, " "));
        assert_eq!(sc.eat_token(), lx!(BraceOpen, "{"));
        assert_eq!(sc.eat_token(), lx!(BraceClose, "}"));
    }

    #[test]
    fn test_peek_whitespace() {
        let mut sc = Scanner::new("h3 h4#h5.h6 {}");
        sc.peek_non_whitespace();
        assert_eq!(sc.eat_token(), lx!(Id, "h3"));
        sc.peek_non_whitespace();
        assert_eq!(sc.eat_token(), lx!(Id, "h4"));
        sc.peek_non_whitespace();
        assert_eq!(sc.eat_token(), lx!(Hash, "#"));
        sc.peek_non_whitespace();
        assert_eq!(sc.eat_token(), lx!(Id, "h5"));
        sc.peek_non_whitespace();
        assert_eq!(sc.eat_token(), lx!(Dot, "."));
        sc.peek_non_whitespace();
        assert_eq!(sc.eat_token(), lx!(Id, "h6"));
        sc.peek_non_whitespace();
        assert_eq!(sc.eat_token(), lx!(BraceOpen, "{"));
        sc.peek_non_whitespace();
        assert_eq!(sc.eat_token(), lx!(BraceClose, "}"));
    }

    #[test]
    fn test_whitespace_mix() {
        let mut sc = Scanner::new("h3 h4#h5.h6 {}");
        assert_eq!(sc.eat_token(), lx!(Id, "h3"));
        assert_eq!(sc.eat_token(), lx!(Whitespace, " "));
        assert_eq!(sc.eat_token(), lx!(Id, "h4"));
        assert_eq!(sc.eat_token(), lx!(Hash, "#"));
        assert_eq!(sc.eat_token(), lx!(Id, "h5"));
        assert_eq!(sc.eat_token(), lx!(Dot, "."));
        assert_eq!(sc.eat_token(), lx!(Id, "h6"));
        sc.peek_non_whitespace();
        assert_eq!(sc.eat_token(), lx!(BraceOpen, "{"));
        assert_eq!(sc.eat_token(), lx!(BraceClose, "}"));
    }
}
