# Filters

Now we've seen how to identify parts of a page to turn into data,
let's look at how to manipulate that data.  `scrapelect` does this
using **filters**.  We've seen a couple already, like `take` and `text`,
but let's look at them more closely.

Every filter takes in a value, and a list of *named arguments* (which can
be empty), and returns the result of applying that filter.

We call a filter with `value | filter_name(arg: value, ...)`. The simplest filter is the `id` filter, which takes the value and
returns that same value: if we let `a: 5 | id();`, we get
`{ "a": 5 }`.

Another useful filter is the `dbg` filter, which, like `id`,
returns the original value, but it prints the value to the
terminal as well.  `dbg` has an *optional argument* msg, a
String, which specifies a message to include, if set.  If not
provided to the filter, it prints `debug message: ...`.

```scrp
a: 1 | dbg();
b: 2 | dbg(message: "from b");
```

will output to the console:

```
debug message: 1
from b: 2
```

and return `{ "a": 1, "b": 2 }`.

## Modifying filters

Often, though, it is useful to use a filter that modifies the passed
value in some way.  One useful filter is `strip`, which trims
leading and trailing whitespace from a string, which is often found
inside HTML elements.

```scrp
trimmed: "    hellooooo   " | strip();
```

outputs

```json
{
  "trimmed": "hellooooo"
}
```

Note that this doesn't *mutate* the original value passed in;
that is, if you read another binding as the input to a filter,
applying the filter will not change the value in the original binding:

```scrp
bind: "5";
new: $bind | int();
```

results (where `int` converts the value into the integer type):

```json
{
  "bind": "5",
  "new": 5
}
```

where `bind` is still a string but `new` is an int.

## Filter documentation

Documentation for all of the built-in filters is available at
[docs.rs](https://docs.rs/scrapelect/latest/scrapelect/interpreter/filter/builtin.html),
which lists filter signatures, descriptions, and examples.

## Chaining filters

Filters are designed to be executed in a pipeline, passing the
output of one filter to the input of another:

```scrp
is-not-five: "5" | int() | eq(to: 5) | not();
```

outputs

```json
{
  "is-not-five": false
}
```

where `eq` returns whether the value is equal to `to`, and `not`
returns the boolean NOT (opposite) of the input value.

## Qualifiers

Similar to element blocks, filters can have qualifiers at the end.
The same qualifiers (none for one, `?` for optional, and `*` for list)
can be placed at the end of a filter call to modify its behavior:

- `value | filter(...)?` applies the filter to `value` if it is not `null`,
  or returns `null` if it is.
- `value | filter(...)*`, when `value` is a list, returns the list
  of every element in the list with the filter applied to it
  (similar to the map operation in other languages).  It is an
  error to use the `*` qualifier if the input value is not a list.

### Example

```scrp
floats: "1 2.3 4.5" | split() | float()*;
optional: "3.4" | float()?;
optional2: null | float()?;
```

returns, where `split` turns a string into an array of strings
split on a delimiter (by default, whitespace), and `float` turns
a value into the float type:

```json
{
  "floats": [1.0, 2.3, 4.5],
  "optional": 3.4,
  "optional2": null
}
```
