import Data.Char
import System.IO
import Text.Read
import Prelude hiding (lookup)
import Data.Map

data Type  =  Tvar Int | Tfun Type Type | T_error String       deriving Eq
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

create_constraints (Evar x) g cons c = (Tvar c, ((g ! x, Tvar c):cons), (c+1))
create_constraints (Eabs x e) g cons c = (Tfun s t, cons', c')
  where 
    s = Tvar c
    g' = insert x s g
    (t, cons', c') = create_constraints e g' cons (c+1)
create_constraints (Eapp e1 e2) g cons c = (Tvar c'', (t, Tfun s (Tvar c'')):cons'', (c''+1))
  where 
    (t, cons', c') = create_constraints e1 g cons c
    (s, cons'', c'') = create_constraints e2 g cons' c'

replace (Tvar x) (left,right) = 
  if (x == left) 
    then right 
  else 
    (Tvar x)
replace (Tfun f1 f2) (left,right) = Tfun (replace f1 (left,right)) (replace f2 (left,right))

replace_loop [] (_,_) = []
replace_loop ((s,t):c) (left,right) = ((replace s (left,right),replace t (left,right)):replace_loop c (left,right))

check_if_belongs (Tvar c1,Tvar c2) = 
  if (c1 == c2) 
    then True 
  else False
check_if_belongs (Tvar c,(Tfun e1 e2)) = 
  if (check_if_belongs (Tvar c,e1) || check_if_belongs (Tvar c,e2)) 
    then True 
  else False

unifier [] = []
unifier ((Tvar n,t):c) =
  if (Tvar n == t) 
    then unifier c
  else if (not (check_if_belongs (Tvar n,t))) 
    then ((Tvar n,t):unifier (replace_loop c (n,t)))
  else 
    [(T_error "error",T_error "error")]
unifier ((t,Tvar n):c) =
  if (t == Tvar n) then unifier c
  else if (not (check_if_belongs (Tvar n,t))) 
    then ((Tvar n,t):unifier (replace_loop c (n,t)))
  else 
    [(T_error "error",T_error "error")]
unifier ((Tfun e1 e2,Tfun e3 e4):c) =
  if (Tfun e1 e2 == Tfun e3 e4) 
    then unifier c
  else 
    ((e1,e3):(e2,e4):unifier c)

findTypeError [] = False
findTypeError ((T_error "error",T_error "error"):lt) = True
findTypeError (lh:lt) = findTypeError lt

unify cons = -- repeat unifier until cons is the same
  if findTypeError unified_cons 
    then [(T_error "error",T_error "error")]
  else if unified_cons == cons 
    then unified_cons
  else
    unify unified_cons
  where 
    unified_cons = unifier cons

replace_type eType [(T_error "error",T_error "error")] = (T_error "error")
replace_type (Tfun s t) cons = (Tfun s' t')
  where 
      s' = replace_type s cons
      t' = replace_type t cons
replace_type (Tvar c) [] = (Tvar c)
replace_type (Tvar c) ((left,right):cons) =
  if ((Tvar c) == left) 
    then right
  else
    replace_type (Tvar c) cons

replace_type_loop eType cons =
    if (replaced == eType)
        then replaced
    else
        replace_type_loop replaced cons
    where
        replaced = replace_type eType cons

correct_types (Tvar n) c correct_dict = 
  if (member n correct_dict) 
    then (c, Tvar (correct_dict ! n), correct_dict)
  else 
    (c+1, Tvar c, (insert n c correct_dict))
correct_types (Tfun s t) c correct_dict = (c'', Tfun s' t', correct_dict'')
  where
    (c', s', correct_dict') = correct_types s c correct_dict
    (c'', t', correct_dict'') = correct_types t c' correct_dict'

check_error eType [(T_error "error",T_error "error")] = (42,(T_error "error"),empty)
check_error eType cons = correct_types (replace_type_loop eType cons) 0 empty

readOne  =  do  s <- getLine
                let e = read s :: Expr
                    (eType, cons, _) = create_constraints e empty [] 0
                    solved_cons = unify cons
                    (_,solution,_) = check_error eType solved_cons                   
                return (solution)

count n m  =  sequence $ Prelude.take n $ repeat m

output_types [] = putStr ""
output_types (T_error "error":t) = do putStrLn "type error"
                                      output_types t
output_types (h:t) = do print h
                        output_types t

main     =  do  n <- readLn
                types <- count n readOne
                output_types types