# Advanced Features

`scrapelect` also contains features that make the language more expressive for
selecting and manipulating data.  Note that these can also increase the complexity
of the program if used in excess, so it's recommended to only use these features
as needed.

## URL Recursion

Sometimes, a page contains links to another subpage, and it's necessary to follow
that link to obtain the desired data.  With `scrapelect`'s **URL recursion**, it's
possible to capture this pattern and select elements from a linked page:

Let's take the following pages as an example:

`https://your-url.com/index.html`

```html
<!DOCTYPE html>
<html>
    <!-- ... -->
    <body>
        <p id="story">
            There once lived a great animal, which was great and also an animal.
        </p>
        <a id="next" href="page2.html">Continue</a>
    </body>
</html>
```

`https://your-url.com/page2.html`

```html
<!DOCTYPE html>
<html>
    <!-- ... -->
    <body>
        <p id="story">
            This animal, which was great, was a great animal. The end.
        </p>
    </body>
</html>
```

Let's say we want to get both chapters of this lovely book.  With URL recursion,
we can!

```scrp
next-page-link: #next {
  link: $element | take(key: "href");
} | take(key: "link");

page-1: #story {
  content: $element | text();
};

page-2: <$next-page-link> #story {
  content: $element | text();
};
```

By specifying the URL before the selector in the `page-2` element block, we tell
the `scrapelect` interpreter to read the page from the URL stored in `next-page-link`
and select `#story` from that document.  Thus, this will output:

```json
{
  "next-page-link": "page2.html",
  "page-1":  "There once lived a great animal, which was great and also an animal.",
  "page-2":  "This animal, which was great, was a great animal. The end."
}
```

Both relative URLs (like `page-2.html` and `/from-page-root.html`) are supported,
as well as absolute URLs (like `https://your-url.com/page1.html`).

Note that the URL to recurse on is actually an *inline value* (more in the next
section), so it is valid to have a filter chain, and the URL that `scrapelect` will
use is the result of the filter pipeline. The final type of the value must
be a String (recursion over lists of strings is not currently supported).

## Inline values

Like above, an **inline value** is a value and filter chain enclosed in diamond
brackets: `<value | filter() | filter() | ...>`, and can be used in most places
where a value is expected (filter arguments, URL recursion; not supported in a
`value: <inline>` expression because the diamond brackets are superfluous). It is
equivalent to writing `intermediate: (inline-contents);` and then using `$intermediate`
in place of the inline.  The difference, though, is that inline evaluations are not
returned in the final output of a block.

### Example

```scrp
result: 5 | is_in(list: <"1 2 3 4 5" | split() | int()*>);
```

prints

```json
{
  "result": true
}
```

and its equivalent

```scrp
intermediate: "1 2 3 4 5" | split() | int()*; // [1, 2, 3, 4, 5]
result: 5 | is_in(list: $intermediate);
```

prints

```json
{
  "intermediate": [1, 2, 3, 4, 5],
  "result": true
}
```

It is often more expressive to not use inline values, and is more efficient when
you need to use the same calculation multiple times.  However, inlines are useful
to hide intermediate evaluations that are only used once.

Additionally, note that it is not valid to start an element context inside an inline
value.  If you need to do this, create an intermediate binding.

## Select filters

When you have a list, it is often useful to filter it so that it only contains
elements that have some property.  This is not possible to express with the `*`
qualifier and filters alone, but `scrapelect` has a special kind of filter: the
**select filter**.

The syntax of this filter is `list | [name: value (| filters() | ...)]`.  `name` is any
identifier (usually `item`), where the `scrapelect` interpreter will provide `$name`
as each item in the list while evaluating the `value` pipeline.  The final result
of this pipeline must be a Bool, and determines whether to keep the item in the output
list: if it is `true`, it is returned, if `false`, it is discarded.

That may be a little abstract, so let's see an example:

```scrp
// ["me", "my", "oh", "my"]
list: "me my oh my" | split();
m-words: "me my myself mother mom meow" | split();
// select all items that are equal to "oh"
oh: $list | [ item: $item | eq(to: "oh") ];
// select all items that are in our list of m words
only-ms: $list | [ item: $item | is_in(list: $m-words) ];
nothing: $list | [ item: $item | eq(to: "wow") ];
```

will output:

```json
{
  "list": ["me", "my", "oh", "my"],
  "m-words": ["me", "my", "myself", "mother", "mom", "meow"],
  "oh": ["oh"],
  "only-ms": ["me", "my", "my"],
  "nothing": []
}
```

The order of the original items is preserved.  Note that the result of the `$item`
filter chain *must* be a Bool; it may be helpful to use the
[`truthy`](https://docs.rs/scrapelect/latest/scrapelect/interpreter/filter/builtin/fn.truthy.html)
filter to convert to a boolean.
