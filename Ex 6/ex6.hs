import Data.Char
import System.IO
import Text.Read
import Prelude hiding (lookup)
import Data.Map

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Main program

crConstrList (Evar x) g cons c = (Tvar c, ((g ! x, Tvar c):cons), (c+1))
crConstrList (Eabs x e) g cons c = let
    s = Tvar c
    g' = insert x s g
    (t, cons', c') = crConstrList e g' cons (c+1)
  in (Tfun s t, cons', c')
crConstrList (Eapp (Evar y) e) g cons c = let
    (s, cons', c') = crConstrList e g cons c
    t = Tvar c'
  in (t, ((g ! y, Tfun s t):cons'), (c'+1))
crConstrList (Eapp e1 e2) g cons c = let
    (t, cons', c') = crConstrList e1 g cons c
    (s, cons'', c'') = crConstrList e2 g cons' c'
  in (Tvar c'', (t, Tfun s (Tvar c'')):cons'', (c''+1))

replace [] (_,_) = []
replace ((ch1,ch2):ct) (a,a') = ((replaceInTfun ch1 (a,a'),replaceInTfun ch2 (a,a')):replace ct (a,a'))

replaceInTfun (Tvar x) (a,a') = if x == a then a' else Tvar x
replaceInTfun (Tfun f1 f2) (a,a') = Tfun (replaceInTfun f1 (a,a')) (replaceInTfun f2 (a,a'))

findIn (Tvar t1,Tvar t2) = if t1 == t2 then True else False
findIn (Tvar t1,(Tfun f1 f2)) = if (findIn (Tvar t1,f1) || findIn (Tvar t1,f2)) then True else False

repeatUnify l =
  if checkForError ul then [(Tvar (-1),Tvar (-1))]
  else if ul == l then ul
  else repeatUnify ul
  where ul = unify l

checkForError [] = False
checkForError ((Tvar (-1),Tvar (-1)):lt) = True
checkForError (lh:lt) = checkForError lt

unify [] = []
unify ((Tvar n1,t2):c) =
  if (Tvar n1 == t2) then unify c
  else if (not (findIn (Tvar n1,t2))) then ((Tvar n1,t2):unify (replace c (n1,t2)))
  else [(Tvar (-1),Tvar (-1))]
unify ((t1,Tvar n2):c) =
  if (t1 == Tvar n2) then unify c
  else if (not (findIn (Tvar n2,t1))) then ((Tvar n2,t1):unify (replace c (n2,t1)))
  else [(Tvar (-1),Tvar (-1))]
unify ((Tfun f1 f2,Tfun f1' f2'):c) =
  if (Tfun f1 f2 == Tfun f1' f2') then unify c
  else ((f1,f1'):(f2,f2'):unify c)

finalReplace (Tvar x) cons = findInCons cons (Tvar x) cons
finalReplace (Tfun f1 f2) cons = Tfun (finalReplace f1 cons) (finalReplace f2 cons)

findInCons [] x _ = x
findInCons ((h1,h2):consT) x cons = if x == h1 then finalReplace h2 cons else findInCons consT x cons

fixOrder (Tvar n) c replaced = if (member n replaced) then (c, Tvar (replaced ! n), replaced)
  else (c+1, Tvar c, (insert n c replaced))
fixOrder (Tfun t1 t2) c replaced = let
    (c', t1', replaced') = fixOrder t1 c replaced
    (c'', t2', replaced'') = fixOrder t2 c' replaced'
  in (c'', Tfun t1' t2', replaced'')

finalResult e_type solved_cons =
  if solved_cons == [(Tvar (-1),Tvar (-1))] then Tvar (-1)
  else let (_,result,_) = fixOrder expr 0 empty in result
    where expr = finalReplace e_type solved_cons

printResult [] = putStr ""
printResult (Tvar (-1):lt) = do {putStrLn "type error"; printResult lt}
printResult (lh:lt) = do {print lh; printResult lt}

readOne  =  do  s <- getLine
                let e = read s :: Expr
                    (eType, cons, _) = crConstrList e empty [] 0
                    solved_cons = repeatUnify cons
                    solution = finalResult eType solved_cons                    
                --putStrLn ("Parsed: " ++ show e)
                return (solution)

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                l <- count n readOne
                printResult l
