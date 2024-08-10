use core::fmt;

use std::error::Error as StdError;

/// A specialized [`Result`](core::result::Result) type using [`Error`].
pub type Result<T> = core::result::Result<T, Error>;

/// Error type for scrapelect's interpreter and filters.
#[non_exhaustive]
#[derive(Debug)]
pub enum Error {
    /// A previous error variant re-wrapped with a note.
    ///
    /// Eventually, the goal is to add a span with line numbers for better
    /// error reporting.
    ///
    /// See [`WrapExt::wrap`] to wrap a [`Result`].
    Wrapped {
        // TODO: add span
        // span: Span,
        /// The note to annotate this error with.
        note: String,
        /// The [`Error`] type to be wrapped.
        inner: Box<Error>,
    },
    /// A non-[`Error`] error type that implements [`std::error::Error`] and `Send` and `Sync`.
    ///
    /// See [`MessageExt`] to create this from an appropriate [`Result`].
    Other {
        /// The error message for this error.
        message: String,
        /// An optional inner error that implements [`std::error::Error`] + `Send + Sync`.
        source: Option<Box<dyn StdError + Send + Sync + 'static>>,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Wrapped { note, inner } => {
                writeln!(f, "{inner}")?;
                write!(f, "{note}")?;

                Ok(())
            }
            Error::Other { message, source } => {
                write!(f, "{message}")?;
                if let Some(source) = source {
                    write!(f, ": {source}")?;
                }
                Ok(())
            }
        }
    }
}

/// Easily create an [`Error`] from an inner format string and optional
/// [`std::error::Error`] type.
///
/// # Examples
///
/// With no inner error:
///
/// ```rust
/// # use scrapelect_filter_types::{Error, other};
/// let error = other!("you must be {} in. tall to ride.  you are {} in.", 48, 40);
/// let message = String::from("you must be 48 in. tall to ride.  you are 40 in.");
///
/// assert!(matches!(error, Error::Other {
///     message,
///     source: None,
/// }));
/// ```
///
/// With an inner error.
///
/// ```rust
/// # use scrapelect_filter_types::{Error, other};
/// let to_parse = "abcdef";
/// let source = to_parse.parse::<i32>().unwrap_err();
/// let error = other!(@source, "couldn't parse integer `{to_parse}`");
///
/// let message = String::from("couldn't parse integer `abcdef`");
/// assert!(matches!(error, Error::Other {
///     message,
///     source: Some(..),
/// }));
/// ```
#[macro_export]
macro_rules! other {
    (@Option: $err:expr, $($tt:tt)*) => {
        $crate::Error::Other {
            message: format!($($tt)*),
            source: $err,
        }
    };

    (@$err:expr, $($tt:tt)*) => {
        $crate::other!(@Option:Some(Box::new($err)), $($tt)*)
    };

    ($($tt:tt)*) => {
        $crate::other!(@Option: None, $($tt)*)
    };
}

/// Exit early out of a function with an [`other!`] Error variant.  Equivalent
/// to `return other!(...)`, so the function must have type [`Result`].
#[macro_export]
macro_rules! bail {
    ($($tt:tt)*) => {
        return Err($crate::other!($($tt)*))
    };
}

impl std::error::Error for Error {}

impl Error {
    /// Creates a new [`Error`] with the given `message`.
    #[inline]
    #[must_use]
    pub fn message(message: String) -> Self {
        Self::Other {
            message,
            source: None,
        }
    }

    /// Wrap `self` into a `Wrapped` variant with the given `note`.
    #[inline]
    #[must_use]
    pub fn wrap(self, note: String) -> Self {
        Self::Wrapped {
            note,
            inner: Box::new(self),
        }
    }

    /// Creates an `Other` variant with the given `message` and `source`.
    #[inline]
    #[must_use]
    pub fn other(message: String, source: Box<dyn StdError + Send + Sync + 'static>) -> Self {
        Self::Other {
            message,
            source: Some(source),
        }
    }
}

/// Helper trait to provide the [`msg`](MessageExt::msg)
/// and [`with_msg`](MessageExt::with_msg) methods available on [`Result`]
/// and [`Option`].
pub trait MessageExt {
    type Wrapped: Sized;

    /// If `self` is `Ok` or `Some`, keep that.  Otherwise, return
    /// `Err(Error::Other)` with the given message, and the inner `E`
    /// if `Self = Result<T, E>`, otherwise `None`.
    #[allow(clippy::missing_errors_doc)]
    fn msg<D: fmt::Display>(self, message: D) -> Self::Wrapped;
    /// Equivalent to [`MessageExt::msg`] but lazily calls the `message` function
    /// when necessary.
    #[allow(clippy::missing_errors_doc)]
    fn with_msg<D, F>(self, message: F) -> Self::Wrapped
    where
        D: fmt::Display,
        F: FnOnce() -> D;
}

impl<T, E: StdError + Send + Sync + 'static> MessageExt for core::result::Result<T, E> {
    type Wrapped = Result<T>;

    fn msg<D: fmt::Display>(self, message: D) -> Self::Wrapped {
        match self {
            Ok(t) => Ok(t),
            Err(source) => Err(Error::Other {
                message: message.to_string(),
                source: Some(Box::new(source)),
            }),
        }
    }

    fn with_msg<D, F>(self, message: F) -> Self::Wrapped
    where
        D: fmt::Display,
        F: FnOnce() -> D,
    {
        match self {
            Ok(t) => Ok(t),
            Err(source) => Err(Error::Other {
                message: message().to_string(),
                source: Some(Box::new(source)),
            }),
        }
    }
}

impl<T> MessageExt for Option<T> {
    type Wrapped = Result<T>;

    fn msg<D: fmt::Display>(self, message: D) -> Self::Wrapped {
        self.ok_or(Error::message(message.to_string()))
    }

    fn with_msg<D, F>(self, message: F) -> Self::Wrapped
    where
        D: fmt::Display,
        F: FnOnce() -> D,
    {
        self.ok_or_else(|| Error::message(message().to_string()))
    }
}

/// Helper trait to implement [`wrap`](WrapExt::wrap) and [`wrap_with`](WrapExt::wrap_with) on [`Result`].
pub trait WrapExt<T> {
    /// Wraps `self` with the given message into a `Error::Wrapped` variant.
    #[allow(clippy::missing_errors_doc)]
    fn wrap<D: fmt::Display>(self, message: D) -> Result<T>;
    /// Wraps `self` with the given message into a `Error::Wrapped` variant.
    ///
    /// Equivalent to [`WrapExt::wrap`], but calls the provided function lazily.
    #[allow(clippy::missing_errors_doc)]
    fn wrap_with<D, F>(self, message: F) -> Result<T>
    where
        D: fmt::Display,
        F: FnOnce() -> D;
}

impl<T> WrapExt<T> for Result<T> {
    fn wrap<D: fmt::Display>(self, message: D) -> Result<T> {
        match self {
            Ok(t) => Ok(t),
            Err(e) => Err(e.wrap(message.to_string())),
        }
    }

    fn wrap_with<D, F>(self, message: F) -> Result<T>
    where
        D: fmt::Display,
        F: FnOnce() -> D,
    {
        match self {
            Ok(t) => Ok(t),
            Err(e) => Err(e.wrap(message().to_string())),
        }
    }
}
