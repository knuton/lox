module Text.Lox.Writers.HTML (asHTML) where

import Text.Lox.Types

asHTML :: Fml -> String

asHTML (Atom id) = [id]
asHTML (Eq t1 t2) = termAsHTML t1 ++ " = " ++ termAsHTML t2
asHTML (Neq t1 t2) = termAsHTML t1 ++ " &ne; " ++ termAsHTML t2
asHTML (Pred id ts) = application id ts
asHTML (Not fml) = "&not;" ++ asHTML fml
asHTML (Forall id fml) = "&forall;" ++ [id] ++ " " ++ asHTML fml
asHTML (Exists id fml) = "&exist;" ++ [id] ++ " " ++ asHTML fml
asHTML (And f1 f2) = parens (asHTML f1 ++ " &and; " ++ asHTML f2)
asHTML (Or f1 f2) = parens (asHTML f1 ++ " &or; " ++ asHTML f2)
asHTML (OnlyIf f1 f2) = parens (asHTML f1 ++ " &rarr; " ++ asHTML f2)
asHTML (Iff f1 f2) = parens (asHTML f1 ++ " &harr; " ++ asHTML f2)
asHTML (Xor f1 f2) = parens (asHTML f1 ++ " &oplus; " ++ asHTML f2)
asHTML (Diamond fml) = "&loz;" ++ asHTML fml
asHTML (Box fml) = "&#9633;" ++ asHTML fml

termAsHTML :: Term -> String
termAsHTML (Var id) = [id]
termAsHTML (Fn id ts) = application id ts
termAsHTML (Operation op t1 t2) = termAsHTML t1 ++ operatorEntity op ++ termAsHTML t2

operatorEntity :: Char -> String
operatorEntity '+' = "+"
operatorEntity '-' = "&minus;"
operatorEntity '*' = "&sdot;"
operatorEntity '/' = "&divide;"

-- | Helpers

application id ts = id:'(':(foldl1 (\ x y -> x ++ ", " ++ y) (map termAsHTML ts)) ++ ")"

parens str = "(" ++ str ++ ")"
