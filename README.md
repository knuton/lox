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
expressions. There are several possibilities for writing most logical operations,
including Unicode characters.

Some examples of formulas that would be processed by lox are `forall x. x=x`
(equivalently `/\x.x=x`), `p -> q -> p` and `[](p -> q) => ([]p → []q)`.

lox is unreasonably forgiving when parsing the language. If you use an identifier
as both an object variable and a predicate variable, or mix modal and first-order
operators, it will assume that you know what you are doing and happily parse it.

### Terms

Any upper- or lowercase letter can serve as an identifier for a variable or
function. Below, `x` is any such letter, as is `f`, and `t1, ..., tn` are terms.

```
Term ::= x ¦ f(t1, ..., tn) ¦ t1 + t2 ¦ t1 - t2 ¦ t1 * t2 ¦ t1 / t2
```

### Classical Logic

Any upper- or lowercase letter can serve as a propositional letter or predicate
symbol. Below, `p` is any such letter, as is `P`, `t1, ..., tn` are terms, and `A`
`B` are formulas.

```
Atom ::= p ¦ P(t1, ..., tn) ¦ t1 = t2 ¦ t1 /= t2
```

```
Negation ::= ~A ¦ ¬A
```

```
Conjunction ::= A && B ¦ A & B ¦ A ∧ B
```

```
Disjunction ::= A || B ¦ A | B ¦ A ∨ B
```

```
Implication ::= A -> B ¦ A => B ¦ A → B
```

```
Biconditional ::= A <-> B ¦ A <=> B ¦ A ↔ B
```

```
Quantification ::= forall x. A ¦ /\x. A ¦ ∀x. A ¦ exists x. A ¦ \/x. A ¦ ∃x. A
```

### Modal Logic

```
Modal ::= []A ¦ ◻A ¦ □A ¦ <> A ¦ ⋄A ¦ ◊A ¦ ◇A ¦ ♢A
```

### Formulas

A `Formula` is any of the above, i.e. any of `Atom`, `Negation`, `Conjunction`,
`Disjunction`, `Implication`, `Biconditional`, `Quantification`, or `Modal`.

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

(C) 2013 Johannes Emerich

For licensing, see `LICENSE`.
