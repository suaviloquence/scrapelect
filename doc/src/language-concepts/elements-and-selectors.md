# Elements and Selectors

## Element

An **element** is a special kind of Value that is used to scrape and get
information about a part of a web page. An element represents an HTML element,
such as `<img src="https://cdn2.thecatapi.com/images/edq.jpg" />` or
`<h1>My title</h1>` or many others.  An element value is only valid within an
**element context**, a block of statements where the special binding
`$element` contains the element.

### Selecting an element

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

### Creating an element context

An element context is a selector block with a list of statements that evaluates
to a nested structure when interpreted.  Inside the block, the binding `$element`
provides access to the element specified by the selector.

#### Example

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

### Nested contexts

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

#### Scope

### Element lifetime

[^pseudoclass-caveat]: Certain CSS features, like pseudoclasses and attribute
  selectors, are not currently supported in `scrapelect`.
