# Statements and Values

## Statements

**Statements** are the basic building block of `scrapelect`
programs.  At its core, a statement is a binding `name: value;`.
This means "store the `value` into the name `name`" at the
current scope in the program.

When the program finishes, the `scrapelect` interpreter will output
structured data as defined by the statements in the program.

### The simplest kind of statement

Take this short program, for example:

```scrp
cat-says: "meow";
```

This program consists of one statement, which binds the
name `cat-says` to the value `"meow"`.  When `scrapelect` runs
this program, it will output the following JSON:

```json
{
  "cat-says": "meow"
}
```

This is the core of a `scrapelect` program: assigning values
to names to be outputted as a structure of data.  But what
exactly can a value be?

## Values

In `scrapelect`, every value has a type.  These types are currently

- `Int`: an integer (such as -1, 0, or 48)
- `Float`: a floating-point (decimal) number (such as 1.0, 0.6931, or -7.29)
- `String`: a string of text characters, such as ("hello!", "meow", or "" (the empty string))
- `Bool`: a Boolean value of true or false
- `List`: an ordered collection of other values, such as (`[1, "hello!", 1.0]` or `[]` (the empty list))
- `Structure`: a nested structure of values, where each value is bound to a
  String key (like `{ greeting: "hi there" }` or `{}` (the empty structure))
- `Null`: represents a value of nothing
- `Element`: an HTML element that can be queried and scraped[^element-caveat]

### Constants

The simplest type of value is a constant.  A constant is a type of value
determined when writing the `scrapelect` program, not dependent on
other variables or the state of execution.  There are three ways
to specify a constant:

- A string constant, wrapped in double quotes, such as `my-string: "hello!"`
- An integer constant as a number literal, such as `one: 1;`
- A floating-point constant as a number literal with a decimal
  point, such as `half: 0.5;`

Currently it is not possible to have a list or structure constant,
but this may be added in future versions of `scrapelect`.

### Reading bindings

A value can also be created by reading the value of a previous
binding, with `$name`, where `name` is the name of the previous
binding.

#### Example

```scrp
greeting: "hello!";
message: $greeting;
```

will output:

```json
{
  "greeting": "hello!",
  "message": "hello!"
}
```

Note that `scrapelect` programs are executed in sequential order, so
you can only read bindings that were defined above the statement
that is using them.  Also, only bindings in the current or
more outer scopes can be accessed like this (which will be covered
more in depth in the section on element contexts).

The next (and maybe most important) type of value is the [`Element`](./elements-and-selectors.md)
which will let us read data from a web page, explained in the next section.

[^element-caveat]: Elements are only available inside an *element context*
  and are not outputted in the final result.
