paper:
	./lox -f markdown+lhs+simple_tables -o report.pdf --template=report/template.tex --latex-engine=xelatex report/intro.lhs \
		Text/Lox.lhs Text/Lox/Parsing.lhs Text/Lox/Types.lhs Text/Lox/Reader.lhs \
		Text/Lox/Readers/Document.lhs Text/Lox/Writers/LaTeX.lhs Text/Lox/Writers/HTML.lhs \
		report/fin.lhs

debug:
	./lox -f markdown+lhs+simple_tables -t latex --template=report/template.tex report/intro.lhs \
		Text/Lox.lhs Text/Lox/Parsing.lhs Text/Lox/Types.lhs Text/Lox/Reader.lhs \
		Text/Lox/Readers/Document.lhs Text/Lox/Writers/LaTeX.lhs Text/Lox/Writers/HTML.lhs \
		report/fin.lhs


build:
	ghc -o lox lox.hs
