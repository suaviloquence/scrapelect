# Quick Start

## Installation

Scrapelect requires the Rust toolchain to install with `cargo`.
If you don't have it installed, you can use [`rustup`](https://rustup.rs/).
With rust and `cargo` installed, run

```
$ cargo install scrapelect
```

to install the `scrapelect` interpreter.

## Your first `scrp`

A `scrapelect` program stored in a `name.scrp` file.  Let's create and edit the file
`article.scrp`:

```scrp
title: .mw-page-title-main {
  content: $text;
};

headings: .mw-heading > * {
  content: $text;
}*;
```

This program describes the data on a web page by how to find it (by CSS selector),
and what we want to do with it (get the text of the title and headings).  A `scrapelect`
program describes a certain web page, so this program works when the page's title is
stored in an HTML element with the class `"mw-page-title-main"` and headings in elements
with the class `"mw-heading"`.  In this case, this will let us scrape Wikipedia articles.

After saving the file to `article.scrp`, let's run it on the [Wikipedia entry for "cat"](https://en.wikipedia.org/wiki/Cat):

```
$ scrapelect article.scrp "https://en.wikipedia.org/wiki/Cat"
```

<details>

<summary>I got an error like `command not found: scrapelect`!</summary>

This means the `scrapelect` executable is not in your `PATH`.  By default,
`cargo` installs binaries to (on Linux) `$HOME/.cargo/bin/scrapelect`.
Try adding the directory `~/.cargo/bin` to your PATH if it is not already present.

The [`rustup` book](https://rust-lang.github.io/rustup/installation/index.html#installation)
may have more information, or try searching "add cargo binaries to `PATH`" for your
operating system.

</details>

Let's see the output for that `scrp`:

```json
{
  "headings": [
    { "content": "Etymology and naming" },
    { "content": "Taxonomy" },
    { "content": "Evolution" },
    { "content": "Domestication" },
    { "content": "Characteristics" },
    { "content": "Size" },
    { "content": "Skeleton" },
    { "content": "Skull" },
    { "content": "Claws" },
    { "content": "Ambulation" },
    { "content": "Balance" },
    { "content": "Coats" },
    { "content": "Senses" },
    { "content": "Vision" },
    { "content": "Hearing" },
    { "content": "Smell" },
    { "content": "Taste" },
    { "content": "Whiskers" },
    { "content": "Behavior" },
    { "content": "Sociability" },
    { "content": "Communication" },
    { "content": "Grooming" },
    { "content": "Fighting" },
    { "content": "Hunting and feeding" },
    { "content": "Play" },
    { "content": "Reproduction" },
    { "content": "Lifespan and health" },
    { "content": "Disease" },
    { "content": "Ecology" },
    { "content": "Habitats" },
    { "content": "Ferality" },
    { "content": "Impact on wildlife" },
    { "content": "Interaction with humans" },
    { "content": "Shows" },
    { "content": "Infection" },
    { "content": "History and mythology" },
    { "content": "Superstitions and rituals" },
    { "content": "See also" },
    { "content": "Notes" },
    { "content": "References" },
    { "content": "External links" }
  ],
  "title": {
    "content": "Cat"
  }
}
```

We've collected the content in each heading in this article, as well as its title,
with just that description.  And it's easily parsable by other programs, too.

---

In the following chapters, we'll examine the language concepts and syntax so that
you can create `scrp`s like this one and obtain structured data from any web page.
