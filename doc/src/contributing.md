# Contributing

`scrapelect` is an open-source project, and we're so excited that you're interested
in contributing!  Development happens on [GitHub](https://github.com/suaviloquence/scrapelect/),
where we use issues to track bugs and feature requests, discussions for help and discussions,
and pull requests for code and documentation contributions and review.

## Reporting a bug

Please create a [GitHub issue](https://github.com/suaviloquence/scrapelect/issues)
that contains the `scrapelect` program, relevant fragments of the input web page,
and error messages, if they exist.

## Contributing code changes

If you are adding a feature, consider discussing it on a GitHub feature request
issue or discussion before opening a pull request, to develop the idea and see if
there is community desire.

When you open a [pull request](https://github.com/suaviloquence/scrapelect/pulls),
for a feature addition or bug fix, make sure to lint your code with

```
$ cargo clippy -- --deny clippy::all --warn clippy::pedantic
```

as this will run in CI, and will block your PR from being merged on failure.

Additionally, make sure to format your code with `cargo fmt`, and make sure all
tests pass with `cargo test`.

### Adding a test

When you add a feature, it's also important to add tests for that.  If it's an
addition to the language, create at least one example input/scrp pair in the
`examples` directory, and add it to the `integration_test!` macro in
`src/interpreter/mod.rs`.

We use `insta` for snapshot testing, so run the test with `cargo t`, and it will
fail at first because there is no baseline to compare it to.  Run `cargo insta revie`
(you may have to `cargo install cargo-insta`), and when the output looks correct,
accept the snapshot, and make sure to check the `examples/scrps/*.snap` into git.

### Writing a new built-in filter.

See the section on writing a new filter in the [extending `scrapelect` chapter](./extending-scrapelect.md).
To add a new builtin filter, add it to `src/interpreter/filter/builtin.rs`,
make sure to add documentation and examples in a doc-comment, and add the filter name
to the `build_map!` macro at the bottom of the file.  It is very helpful to add an
integration test that shows how this filter should work, see the section above for more.


## Enhancing this book

This book is also developed in the `scrapelect` repo, and you can contribute to it
without having to write any code.  The text of the book is in the
[`doc/src/`](https://github.com/suaviloquence/scrapelect/tree/dev/doc/src) folder,
and you can edit each `chapter.md` folder to enhance the documentation and submit
it as a pull request.

While you are developing this, you can use `mdbook serve --open` to view a local copy
of the book that will update with your changes (you may have to `cargo install mdbook`).
