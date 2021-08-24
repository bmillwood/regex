# Interactive Regex Explainer (Regexplainer?)

This tool contains a custom regex parser that parses the subset of regexes I
could remember off the top of my head, and aims to provide tools to understand,
debug, or edit them.

## Features

If you type a regular expression in the text field, the program will attempt to
render an English explanation of your regular expression, that updates in real
time as you type.

In some cases, the explanation is directly editable, and the edits will be
reflected in the regular expression input.

## Planned features

* Editable explanation improvements:
    * Make more kinds of nodes editable.
    * Include the ability to add, remove, or reorder nodes.
* List matches for the expression:
    * the first 10, by some notion of "first"
    * random matches
* Show how the expression matches given input:
    * Show which part of the regex matched which part of the input.
    * For a partial match, show some possible continuations of the match.

## Bugs or missing features in the regular expression parser

* Character classes incorrectly handle special characters and escaping. For
  example, `]` is supposed to be a member of the class instead of ending it if
  it occurs first (or after `^`). I haven't implemented that rule.
* Most "special" backslash-escapes aren't implemented, like `\d` for digit, `\s`
  for whitespace, `\b` for word boundaries. These are easy to implement but I
  just haven't gone through them yet.
* Likewise named classes, like `[[:digit:]]`.
* I don't currently parse greediness qualifiers. They're only relevant in the
  presence of captures anyway, and I'm not currently doing much with captures.
* I don't support backreferences within the pattern, e.g. (famously) `^(11+)\1+`
  that matches a string of 1s whose length is not a prime number. These are
  harder to explain and manipulate. I might not implement them at all.

## Related work

These tools all have the capacity to accept regular expressions, highlight and
describe syntax components, and show matches in some text, along with capture
groups:

* [RegExr](https://regexr.com/)
* [RegEx101](https://regex101.com/)
* [RegEx Testing](https://regex101.com/)
