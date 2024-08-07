# Interpreter CLI

Currently, the command line interface of the `scrapelect` interpreter is very
simple.  The way to invoke the interpreter is:

```
$ scrapelect <scrp_path> <url>
```

where `scrp_path` is the path to the `.scrp` file, and `url` is a qualified,
absolute URL. That is, `url` must start in a scheme (like `https://` or `file://`).

Currently, `scrapelect` supports the following URL schemes:

- `http://`, `https://`: a "typical" URL to a web page.
- `file://`: read a file on the local device, without using the internet.
