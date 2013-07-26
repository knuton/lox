Parsing Formulas (`Text.Lox.Reader`)
------------------------------------

Before going into how formulas are parsed, it makes sense to give the grammar
for the plaintext language in a more readable form.


\subsubsection{Terms}

Any upper- or lowercase letter can serve as an identifier for a variable or
function. Below, `x` is any such letter, as is `f`, and `t1, ..., tn` are terms.

```
Term ::= x ¦ f(t1, ..., tn) ¦ t1 + t2 ¦ t1 - t2 ¦ t1 * t2 ¦ t1 / t2
```

In keeping with tradition, point operations bind strongest.

\subsubsection{Classical Logic}

Any upper- or lowercase letter can serve as a propositional letter or predicate
symbol. Below, `p` is any such letter, as is `P`, `t1, ..., tn` are terms, and
`A`, `B` are formulas.

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
Quantification ::= forall x. A ¦ /\x. A ¦ ∀x. A
                 ¦ exists x. A ¦ \/x. A ¦ ∃x. A
```

The binding strength of logical connectives is unsurprising: all unary
operations bind stronger than binary operations. The binary operations are --
from strongest to weakest binding -- conjunction, disjunction, implication, and
biconditional.

\subsubsection{Modal Logic}

```
Modal ::= []A ¦ ◻A ¦ □A ¦ <> A ¦ ⋄A ¦ ◊A ¦ ◇A ¦ ♢A
```

\subsubsection{Formulas}

A `Formula` is any of the above, i.e. any of `Atom`, `Negation`, `Conjunction`,
`Disjunction`, `Implication`, `Biconditional`, `Quantification`, or `Modal`.

---

This module exports the `statement` parser monad that can be used to parse
formulas in plaintext, resulting in a value of type `Fml`. Many parts of the
parser should be self-explanatory, but there will be some comments where
seemly.

> module Text.Lox.Reader (statement) where

> import Control.Applicative ((<$>), (<*>), (*>), (<*))
> import Text.ParserCombinators.Parsec
> import Text.Lox.Types
> import Text.Lox.Parsing

> statement = formula <* eof

> -- |
> -- | Terms
> -- |

> term :: GenParser Char st Term
> term = try addoperation <|> try fnApp <|> var <?> "term"

> -- | Atomic Terms

> var :: GenParser Char st Term
> var = Var <$> letter

> fn :: GenParser Char st Char
> fn = letter

> fnApp :: GenParser Char st Term
> fnApp = application (Fn <$> fn) term

> -- | Complex Terms

Since the usual arithmetic operations are binary left-associative, some care
needs to be taken to prevent the parser from going into an infinite loop by
infinitely recursing into the left-hand operand, which might be an arithmetic
operation itself, without ever consuming a single character.

The Parsec library function `chainl1` can be used as a solution, by regarding
operations as a series of more specific and therefore unproblematic syntactic
constructs, separated by the operator in question.

> addoperator = Operation <$> spaced (oneOf "+-")
> addoperation = chainl1 addoperand addoperator
>   where
>     addoperand = try muloperation <|> try fnApp <|> var

> muloperator = Operation <$> spaced (oneOf "/*")
> muloperation = try (chainl1 muloperand muloperator) <|> parens muloperation
>   where
>     muloperand = try (parens addoperation) <|> try fnApp <|> var

> -- |
> -- | Formulas
> -- |

> formula :: GenParser Char st Fml
> formula = try biconditional
>        <|> try negation
>        <|> try quantification
>        <|> try modaloperation
>        <|> atom

> -- | Atomic Formulas

> atom = tryAllOf [ binop (symbol "=" *> return Eq) term term
>                 , binop (symbol "/=" *> return Neq) term term
>                 , application (Pred <$> letter) term
>                 , Atom <$> letter
>                 ]

> unary = try quantification
>      <|> try modaloperation
>      <|> try (flipped unary)
>      <|> atom

> -- | Complex Formulas

> -- Quantifiers

> allquant = symbols [ "forall", "/\\", "\x2200" ] *> return Forall
> exquant = symbols [ "exists", "\\/", "\x2203" ] *> return Exists

> quantification = do
>     quantifier <- allquant <|> exquant
>     identifier <- letter
>     spaced (char '.')
>     fml <- try (parens formula) <|> unary
>     return $ quantifier identifier fml

> -- Negation

> negation = flipped formula

> -- Conjunction

As in the case of arithmetic operations, `chainl1` is employed to prevent loops
when parsing binary left-associative connectives.

> wedge :: GenParser Char st (Fml -> Fml -> Fml)
> wedge = symbols [ "&&", "&", "\x2227" ] *> return And

> conjunction = try (chainl1 conjuncts wedge) <|> parens conjunction
>   where conjuncts = try (weakBinding disjunction)
>                  <|> quantification
>                  <|> unary

> -- Disjunction

> vee :: GenParser Char st (Fml -> Fml -> Fml)
> vee = symbols [ "||", "|", "\x2228" ] *> return Or

> disjunction = chainl1 disjuncts vee
>   where disjuncts = try (strongBinding conjunction)
>                  <|> unary

> -- Implication

> arrow :: GenParser Char st (Fml -> Fml -> Fml)
> arrow = symbols [ "->", "=>", "\x2192" ] *> return OnlyIf

> implication = optionalParens (binop (arrow *> return OnlyIf) operand formula)
>   where operand = try (strongBinding disjunction)
>                <|> unary

> -- Biconditional

> doublearrow :: GenParser Char st (Fml -> Fml -> Fml)
> doublearrow = symbols [ "<->", "<=>", "\x2194" ] *> return Iff

> biconditional = chainl1 equivalents doublearrow
>   where equivalents = tryAllOf (map strongBinding [implication, disjunction])
>                    <|> unary

> -- Modal Operators

> diamond, box :: GenParser Char st (Fml -> Fml)
> diamond = symbols [ "<>"
>                   , "\x22C4"
>                   , "\x25CA"
>                   , "\x25C7"
>                   , "\x2662"
>                   ] *> return Diamond
> box = symbols [ "[]", "\x25FB", "\x25A1" ] *> return Box

> modaloperation = do
>     modop <- diamond <|> box
>     fml <- try (parens formula) <|> unary
>     return $ modop fml

> -- | Parsing Formulas

> flipped p = Not <$> (symbols [ "~", "\x00AC" ] *> p)

The functions `weakBinding` and `strongBinding` transform a parser for a
connective into a parser for either a formula with its outermost connective
being the one in question or a negation followed by the connective in question.
In case of the connective being weaker than the connective as an operand of
which it appears, it needs to appear in parentheses in the former case.

> weakBinding p = try (flipped (parens p)) <|> parens p
> strongBinding p = try (flipped (parens p)) <|> p
