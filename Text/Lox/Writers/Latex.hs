module Text.Lox.Writers.Latex (asLatex) where

import Text.Lox.Types

asLatex :: Fml -> String

asLatex (Atom id) = [id]
asLatex (Eq t1 t2) = termAsLatex t1 ++ " = " ++ termAsLatex t2
asLatex (Neq t1 t2) = termAsLatex t1 ++ " = " ++ termAsLatex t2
asLatex (Pred id ts) = application id ts
asLatex (Not fml) = "\\neg " ++ asLatex fml
asLatex (Forall id fml) = "\\forall " ++ [id] ++ ". " ++ asLatex fml
asLatex (Exists id fml) = "\\exists " ++ [id] ++ ". " ++ asLatex fml
asLatex (And f1 f2) = parens (asLatex f1 ++ " \\wedge " ++ asLatex f2)
asLatex (Or f1 f2) = parens (asLatex f1 ++ " \\vee " ++ asLatex f2)
asLatex (OnlyIf f1 f2) = parens (asLatex f1 ++ " \\rightarrow " ++ asLatex f2)
asLatex (Iff f1 f2) = parens (asLatex f1 ++ " \\leftrightarrow " ++ asLatex f2)
asLatex (Xor f1 f2) = parens (asLatex f1 ++ " \\oplus " ++ asLatex f2)
asLatex (Diamond fml) = "\\Diamond " ++ asLatex fml
asLatex (Box fml) = "\\Box " ++ asLatex fml

termAsLatex :: Term -> String
termAsLatex (Var id) = [id]
termAsLatex (Fn id ts) = application id ts
termAsLatex (Operation op t1 t2) = termAsLatex t1 ++ " " ++ latexOperator op ++ " " ++ termAsLatex t2

latexOperator :: Char -> String
latexOperator '+' = "+"
latexOperator '-' = "-"
latexOperator '*' = "\\cdot"
latexOperator '/' = "\\div"

-- | Helpers

application id ts = id:'(':(foldl1 (\ x y -> x ++ ", " ++ y) (map termAsLatex ts)) ++ ")"

parens str = "(" ++ str ++ ")"
