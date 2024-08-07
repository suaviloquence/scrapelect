# Elements and Selectors

An **element** is a special kind of Value that is used to scrape and get
information about a part of a web page. An element represents an HTML element,
such as `<img src="https://cdn2.thecatapi.com/images/edq.jpg" />` or
`<h1>My title</h1>` or many others.  An element value is only valid within an
**element context**, a block of statements where the special binding
`$element` contains the element.

## Selecting an element

In `scrapelect`, we identify an element by its *CSS selector*. This can be simple
(e.g., the tag name: `h1`), or arbitrarily complex because selectors can be
combined to identify any element.  See the [MDN CSS selector reference](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_selectors)
for a full guide on writing CSS selectors[^pseudoclass-caveat].

Common selector patterns:

- `tag`: Selects elements with the tag name `tag`: `<tag></tag>`
- `#id`: Selects the element with ID `id`: `<a id="id"></a>`
- `.class`: Selects elements with CSS class `class`: `<x class="class" />`
- `a#b.c.d`: Combine selectors (without whitespace) to select an element with
  all of these properties: `<a id="b" class="c d">...</a>`
- `a#b .c.d`: Combine selectors *with whitespace* to select an element inside
  a parent element: selecting the `span` in `<a id="b"><span class="c d">...</span></a>`
- ...and many more.  See the [MDN reference](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_selectors) for more.

## Creating an element context

An element context is a selector block with a list of statements that evaluates
to a nested structure when interpreted.  Inside the block, the binding `$element`
provides access to the element specified by the selector.

### Example

On the following fragment:

```html
<a>Not special</a>
<a id="special"">Special</a>
```

```scrp
special: #special {
  text: $element | text();
}
```

will output:

```json
{
  "special": {
    "text": "Special"
  }
}
```

Notice how all bindings in the element context are evaluated into a nested
structure stored in `special`.  (Note: `$element | text()` is calling the `text`
filter on `$element`, which will be explained in the [filters](./filters.md)
chapter, but it means "get the text inside the element `$element`").

## Nested contexts

Inside an element context, there are statements.  And statements can bind names
to element contexts.  Thus, it is valid (and often useful) to have a nested element
context.  Inside a parent element context, a child element block's selector starts
selecting elements inside the parent element: thus, in the following example,
`calico` will be selected, but not `shi tzu`:

```html
<ul id="cats">
    <li>calico</li>
    <!-- ... -->
</ul>
<ul id="dogs">
    <li>shi tzu</li>
    <!-- ... -->
</ul>
```

```scrp
cat: #cats {
  type: li {
    content: $element | text();
  }
}
```

will output

```json
{
  "cat": {
    "type": {
      "content": "calico"
    }
  }
}
```

## Scope

Inside an element context block, it is possible to read bindings from
an outer context if they are declared above the current statement.  However,
if an item exists in a more inner context, it will shadow the outer one.

### Example

```scrp
context: "outer";
outer: "outer";

parent: parent {
  context: "middle";
  child: child {
    context: $context;
    outer: $outer;
  };
};
```

outputs

```json
{
  "context": "outer",
  "outer": "outer",
  "parent": {
    "child": {
      "context": "middle",
      "outer": "outer"
    },
    "context": "middle"
  }
}
```

Note that it is not *directly* possible to read bindings declared in a context more inner than the current,
even if the block is above the current statement, since an element context block is evaluated into a
structure.  However, with filters like `take`, it is possible to read this data, just not
by binding name syntax `$name`.

### Element lifetime

It is possible to rebind the value contained in `$element`.  However, because an element
is only valid inside the element context, these will not be returned in the final output.
In fact, any bindings that contain `$element` at the close of an element block will be
omitted from the returned structure.

#### Example

```scrp
child: a {
  this: $element;
};
unexpected: child | take(key: "this");
```

will output, where `child | take(key: "this")` means "return the value with key `"this"` in the `child`
structure, and return `null` if it is not present:

```json
{
  "child": {},
  "unexpected": null
}
```

Note that `child` is an empty structure, even though it bound `this` to `$element`.

## Selecting multiple elements: qualifiers

By default, an element block will only select the first element that matches
a selector, and raise an error if it is not found.  However, it
is often useful to select all the elements that match a selector,
or select one optional element, not raising an error if it does
not exist.  We can specify how many elements to expect with
**qualifiers**.  A qualifier is placed at the end of an element
context block, and can be one of:

- `` (no qualifier): the default, so select the first element that
  matches this selector, and raises an error if there are none
- `?` (optional): similarly, selects the first element matching the
  selector, but the element context evaluates to `null` instead
  of erroring if there is no element matching that selector
- `*` (all): select all elements matching this selector, evaluate
  the element block for each one, and place the results in a `List`

### Examples

Take the document fragment:

```html
<li>1</li>
<li class="even"">2</li>
<li>3</li>
<li class="even">4</li>
```

Given the `scrp`:

```scrp
// no qualifier (first)
first_num: li {
  text: $element | text();
};

// * qualifier (all)
numbers: li {
  text: $element | text();
}*;

// ? qualifier (optional)
optional: #not-here {
  text: $element | text();
}?;
```

will output:

```json
{
  "first_num": { "text": "1" },
  "numbers": [
    { "text": "1" },
    { "text": "2" },
    { "text": "3" },
    { "text": "4" }
  ],
  "optional": null
}
```


[^pseudoclass-caveat]: Certain CSS features, like pseudoclasses and attribute
  selectors, are not currently supported in `scrapelect`.
