# scrapelect

(S)CSS-based web scraping DSL.  Describe the web page in terms of CSS selectors
and filters on those elements, and extract structured data by scraping and
selecting on a web page.

## examples
```
links: a {
  href: $element | attrs() | take(key: "href");
  link_text: $text;
}*;

content: #content {
  images: img {
    alt_text: $element | attrs() | take(key: "alt");
  }*;

  subtitle: #subtitle {
    subtitle_text: $text;
  }?;
};
```

Querying this on the following page gives this result:

<details>

<summary>example.html</summary>

```html
<!DOCTYPE html>
<html>
    <head></head>
    <body>
        <div id="header">
            <img src="/logo.png" alt="logo" />
            <a href="/home">Home</a>
            <a href="/about">About me</a>
        </div>
        <div id="content">
            <!-- no subtitle on this one -->
            <!-- <h2 id="subtitle">What it would say... -->
            <img src="/cats/calico.png" alt="a calico cat" />
            <img src="/cats/tabby.png" alt="a tabby cat" />
            <a href="/payment-processor">See more cats...</a>
        </div>
    </body>
</html>
```

</details>

```json
{
  "links": [
    { "href": "/home", "link_text": "Home" },
    { "href": "/about", "link_text": "About me" },
    { "href": "/payment-processor", "link_text": "See more cats..." }
  ],
  "content": {
    "images": [
      { "alt_text": "a calico cat" },
      { "alt_text": "a tabby cat" }
    ],
    "subtitle": null
  }
}
```

## syntax

A **scrapelect** program is made of a list of *statements*, which when queried on an HTML page,
outputs structured data by evaluating the statements on the contents of the web page.

A **statement** is of the form `name: rvalue (| filter(...))*;`.
That is, a statement sets an output binding `name` to the result of evaluating an `rvalue` and an optional list of filters.

When a statement is interpreted on an input page, it will output data of the form `{ name: value }`, where `name` is the output
variable name, and `value` is the result from evaluating the `rvalue` and the filter list.

### values

#### constants: strings, ints, and floats

The simplest statement binds a name to a constant.  For example, the statements

```
type: "cat";
coloring: "calico";
age: 9;
weight: 12.5;
```

evaluated on *any* page will output

```json
{
  "type": "cat",
  "coloring": "calico",
  "age": 9,
  "weight": 12.5
}
```

As seen above, a constant can be either a *string* (text enclosed in double quotes), an integer, or a floating-point decimal number.

#### variables

Additionally, statements can reference other, previously defined bindings by using `$name` to read the value of binding `name`.  For example,

```
animal: "cat";
favorite_animal: $animal;
```

will evaluate to:

```json
{
  "animal": "cat",
  "favorite_animal": "cat"
}
```

It is also possible to **shadow** previously-defined bindings by creating a new binding of the same name. In this case, only the newest binding of
a given name will be outputted in the final result: for example,

```
output: "Not me!";
output: "or me,";
output: "but me!";
```

will result in `{ "output": "but me! }`.

#### elements

The above statements will all evaluate the same for any input web page.  To scrape a web page and create structured data from its contents,
use an **element** as a value.  Elements are identified by their *CSS selector*, and evaluating an element on an input HTML document lets
you select, scrape, and structure data about that HTML element.

Inside an element block is another list of statements.  These statements have access to the two special bindings **$element** and **$text**
which provide access to properties of the element and its text content, respectively. Evaluating an element will return a nested structure
containing the bindings inside the element block.

For example, take the input element `<p id="meowing">Meow mrrp meow meow!</p>`.  The CSS selector to identify its element by its ID is `#meowing`.

```
meow_element: #meowing {
  meows: $element | text();
};
```

Running this on the above element will output the following data:

```json
{
  "meow_element": {
    "meows": "Meow mrrp meow meow!"
  }
}
```

It is also possible to have **nested elements**.  An child element inside a parent element block only selects elements inside that parent element, so
with the following input:

```html
<p id="parent">
    <p>This one!</p>
</p>
<p>Not this one :(</p>
```

this scrp

```
parent: #parent {
  child: p {
    child_text: $element | text();
  }
};
```

will evaluate to:

```json
{
  "parent": {
    "child": {
      "child_text": "This one!"
    }
  }
}
```

Note that it is not currently possible to shadow the `text` or `element` bindings inside an element scope.  Statements in an inner element can
access previously-defined bindings in an outer scope, but not vice versa.  That is,

```
cat: "meow";
outer: #outer {
  my_cat: $cat;
};
```

is valid, but

```
outer: #outer {
  cat: "meow";
};
my_cat: $cat:
```

is not.  However, it is possible to access child elements using the `take` filter, which will be discussed later.

##### element qualifiers

### filters





_**scrapelect:** scrape + select, also -lect_
