lox
===

lox is a Haskell library for parsing logic formulas written in a simple
human-readable language. It also has an accompanying executable `lox`, which
wraps around [Pandoc](http://johnmacfarlane.net/pandoc/) to extend
[Markdown](http://daringfireball.net/projects/markdown/) with inline formulas.

Basic support has been implemented for

- propositional logic,
- first-order logic, and
- modal logic.

Language
--------

The notation for formulas understood by lox is meant to be such that it can be
easily written and read in a text editor, while being precise enough for strict
analysis and translation into different formats, most prominently LaTeX math
expressions.

Some examples of formulas that would be processed by lox are `forall x. x=x`,
`/\x.x=x`, `p -> q -> p` and `[](p -> q) -> ([]p -> []q)`.

lox is unreasonably forgiving when parsing the language. If you use an identifier
as both an object variable and a predicate variable, or mix modal and first-order
operators, it will assume that you know what you are doing and happily parse it.

Documents
---------

As with formulas itself, `lox` is very forgiving with documents. If it can not
successfully process some formula, it will leave it unchanged and process only the
remaining formulas. If it fails on the whole document, it will just pass it on to
Pandoc without any changes.

Dependencies
------------

- pandoc

Copyright
---------

Copyright 2013 Johannes Emerich

For licensing, see `LICENSE`.
