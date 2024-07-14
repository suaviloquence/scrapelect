# scrapelect

(S)CSS-based web scraping DSL.  Describe the web page in terms of CSS selectors
and filters on those elements, and extract structured data by scraping and
selecting on a web page.

## examples
```
links: a(*) {
  href: $element | attrs() | take(key: "href");
  link_text: $text;
}

content: #content {
  images: img(*) {
    alt_text: $element | attrs() | take(key: "alt");
  }

  subtitle: #subtitle? {
    subtitle_text: $text;
  }
}
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

TBD, see grammar.txt

_**scrapelect:** scrape + select, also -lect_
