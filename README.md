# scrapelect

`scrapelect` is a web scraping language inspired by CSS that turns
a web page into structured JSON data.  Select elements with CSS
selectors, apply filters to extract and modify the data you want from
a web page, and get the output in a structured, machine-readable,
interoperable format.

## installation

Install the [Rust toolchain](https://rustup.rs/).  Using `cargo`,
run:

```
$ cargo install scrapelect
```

to install the `scrapelect` interpreter.

## usage

Write a `scrapelect` program into a `.scrp` file.  Documentation
for the language can be found in the [`scrapelect` book](https://suaviloquence.github.io/scrapelect/the-book.html).

A quick example, `title.scrp`, retrieves the title of a Wikipedia article:

```scrp
title: .mw-page-title-main {
  content: $element | text();
};
```

Run the `scrp` with the URL of the web page to scrape:

```
$ scrapelect title.scrp "https://en.wikipedia.org/wiki/Cat"
```

It will output:

```json
{
  "title": {
    "content": "Cat"
  }
}
```

## documentation

- [The `scrapelect` book](https://suaviloquence.github.io/scrapelect/the-book.html)
  contains documentation on language concepts and how to write a `scrapelect`
  program.
- Additionally, documentation for scrapelect's built-in filters
  is located at [docs.rs](https://docs.rs/scrapelect/latest/scrapelect/interpreter/filter/builtin)
- Developer-level documentation is also at [docs.rs](https://docs.rs/scrapelect/latest/scrapelect),
  but it is currently incomplete.

## community

- [GitHub issues](https://github.com/suaviloquence/scrapelect/issues)
  and [discussions](https://github.com/suaviloquence/scrapelect/discussions)
  are great places to report bugs, request features, and get help
  using `scrapelect`
- Also, consider submitting a [pull request](https://github.com/suaviloquence/scrapelect/pulls)
  to contribute to the code or documentation.
- See the [contributing](https://suaviloquence.github.io/scrapelect/contributing.html)
  chapter of the `scrapelect` book for more information on contributing to `scrapelect`.

## license

`scrapelect` is available under the MIT or Apache 2 licenses, at your
option.  Copies of these licenses are included at
[LICENSE-MIT](https://github.com/suaviloquence/scrapelect/blob/dev/LICENSE-MIT) and
[LICENSE-APACHE](https://github.com/suaviloquence/scrapelect/blob/dev/LICENSE-APACHE)
at the root directory.



_**scrapelect:** scrape + select, also -lect_
