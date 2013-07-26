# About this Project

The goal of this project was to design a plain text language that makes it easy
to read and write logic formulas using any run of the mill text editor, and to then
develop a software tool, christened \textrm{``Lox''}, for translating those
formulas into different target formats such as LaTeX or HTML. Having some
potential usefulness outside of a pure research project, the original code was
not written in literal style to be more adequate for an open-source release.

This report however is produced as a literate Haskell fork of the original code
base and is compiled using the `lox` executable, which extends the variant of
Markdown *understood* by [Pandoc](http://johnmacfarlane.net/pandoc/) with
statements for logic formulas such as `§p -> q -> p§` which will be translated
to §p -> q -> q§. In an attempt to use Pandoc as a library as much as possible
instead of directly \textrm{``enhancing''} the Pandoc source code, effectively
creating a custom fork, documents are processed in two steps. First statements
marked with `§` are processed and replaced with LaTeX math expressions. In the
second step this preprocessed document is passed on to and processed by Pandoc.

The software can be used as both a library for parsing and converting logic
formulas, as well as a drop-in replacement for `pandoc` if logic formulas are a
desirable extension for authoring a specific document.

The majority of code in `lox.hs` was not written by me but is an adapted version
of the `pandoc` executable to ensure maximal interoperability and should
therefore be disregarded as concerns the study project. Other than this,
however, Lox is independent of Pandoc.

# Modules
