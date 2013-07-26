In Closing
==========

For demonstrative purposes, the following table shows formula statements and
their transformed version.

   Statement                                   Result
   ------------------------------------------- -----------------------------------------
   `§forall x. R(x,x)§`                        §forall x. R(x,x)§
   `§/\x./\y./\z.(R(x,y) & R(y,z) -> R(y,z))§` §/\x./\y./\z.(R(x,y) & R(y,z) -> R(y,z))§
   `§p => []<>p§`                              §p => []<>p§

Although the fact that this document could be prepared with Lox shows that it is
already of some use, there is much left to do if one would want to make it a
reliable and versatile tool.

- Verification of the correctness of parsing and transforming has been done
  manually, which leaves much room for edge cases to behave in an unexpected way
  and regressions to sneak back in. A test suite would be essential in order to
  ensure consistent quality. An obvious first step would be to generate random
  values of the `Fml` type, serialize them to one or several plaintext
  representations, reparse them, and compare the outcome to the original value.
- Currently the only statements that can be parsed are formulas. For example,
  the term statement `§f(x,m)+x§` would not be translated. By introducing a
  typeclass `Statement` and making `Fml` and `Term` instances of it, this could
  be improved.
- The expressiveness of the language could be increased by allowing more
  symbols, as well as subscript and superscript. To give even more flexibility,
  LaTeX math code could be allowed for term and formula atoms.
- As mentioned earlier, the forgiving nature of Lox was a conscious choice, but
  it would be handy to offer several optional strict modes to check for
  well-formedness and other properties.
