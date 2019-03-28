import Data.Char
import Text.Read
import Text.Read.Lex

-- Syntax

type Var = String

data C = Cskip | Cassign Var E | Cseq C C | Cfor E C | Cif E C C | Cwhile E C
data E = Ezero | Esucc E | Epred E | Eif E E E | Evar Var
       | Etrue | Efalse | Elt E E | Eeq E E | Enot E
       | Econs E E | Ehd E | Etl E

-- semantic domains

data Data = Data_Int Integer | Data_Bool Bool | Cell (Data, Data) | Null
type S    = (Var, Data) -> Data

-- semantic functions

semC :: C -> S -> S
semC Cskip s = s
semC (Cassign x n) s = Main.update s (x, Null) (semE n s)
semC (Cseq c1 c2) s = semC c2 (semC c1 s)
semC (Cfor e c) s = expon i (semC c) s
  where i = toInt (semE e s)
semC (Cif e c1 c2) s | b         = semC c1 s
                     | otherwise = semC c2 s
  where Data_Bool b = semE e s
semC (Cwhile e c) s = fix bigF s
  where bigF f s | toBool (semE e s) = f (semC c s)
                 | otherwise = s

semE :: E -> S -> Data
semE Ezero s = Data_Int 0
semE (Esucc e) s = let Data_Int n = semE e s in Data_Int (n + 1)
semE (Epred e) s = let Data_Int n = semE e s in Data_Int (n - 1)
semE (Eif e1 e2 e3) s | b         = semE e2 s
                      | otherwise = semE e3 s
  where Data_Bool b = semE e1 s
semE (Evar v) s = s (v, Null)
semE Etrue s = Data_Bool True
semE Efalse s = Data_Bool False
semE (Elt e1 e2) s = Data_Bool (toInt (semE e1 s) < toInt (semE e2 s))
semE (Eeq e1 e2) s = Data_Bool (toInt (semE e1 s) == toInt (semE e2 s))
semE (Enot e) s = let Data_Bool b = semE e s in Data_Bool (not b)
semE (Econs e1 e2) s = Cell ((semE e1 s), (semE e2 s))
semE (Ehd (Econs e1 e2)) s = semE e1 s
semE (Etl (Econs e1 e2)) s = semE e2 s

-- auxiliary functions

  -- Convert from Data to Integer
toInt (Data_Int n) = n
toInt (Data_Bool b) = toInteger (fromEnum b)
toInt (Cell (n1, n2)) = read (show (toInt n1) ++ show (toInt n2)) :: Integer

  -- Convert from Data to Bool
toBool (Data_Bool b) = b

expon 0 f = id
expon n f = f . expon (n-1) f

update s (v1, d1) n (v2, d2) | v1 == v2  = n
                             | otherwise = s (v2, d2)

fix f = f (fix f)

-- Pretty-printing

instance Show E where
  showsPrec p Ezero = ("0" ++)
  showsPrec p (Esucc n) = ("succ " ++) . showsPrec 2 n
  showsPrec p (Epred n) = ("pred " ++) . showsPrec 2 n
  showsPrec p (Eif e e1 e2) =
    showParen (p > 0) $
    ("if " ++) . showsPrec 1 e . (" then " ++) . showsPrec 1 e1 .
                                 (" else " ++) . showsPrec 0 e2
  showsPrec p (Evar x) = (x ++)
  showsPrec p Etrue = ("true" ++)
  showsPrec p Efalse = ("false" ++)
  showsPrec p (Elt e1 e2) =
    showParen (p > 1) $
    showsPrec 2 e1 . (" < " ++) . showsPrec 2 e2
  showsPrec p (Eeq e1 e2) =
    showParen (p > 1) $
    showsPrec 2 e1 . (" = " ++) . showsPrec 2 e2
  showsPrec p (Enot e) =
    showParen (p > 2) $
    ("not " ++) . showsPrec 2 e
  showsPrec p (Econs e1 e2) =
    showParen (p > 1) $
    showsPrec 2 e1 . (" : " ++) . showsPrec 1 e2
  showsPrec p (Ehd e) =
    showParen (p > 2) $
    ("hd " ++) . showsPrec 2 e
  showsPrec p (Etl e) =
    showParen (p > 2) $
    ("tl " ++) . showsPrec 2 e

instance Show C where
  showsPrec p Cskip = ("skip" ++)
  showsPrec p (Cassign x e) = (x ++) . (" := " ++) . showsPrec 0 e
  showsPrec p (Cseq c1 c2) =
    showParen (p > 0) $
    showsPrec 1 c1 . ("; " ++) . showsPrec 0 c2
  showsPrec p (Cfor e c) =
    showParen (p > 1) $
    ("for " ++) . showsPrec 1 e . (" do " ++) . showsPrec 1 c
  showsPrec p (Cif e c1 c2) =
    showParen (p > 1) $
    ("if " ++) . showsPrec 1 e . (" then " ++) . showsPrec 2 c1 .
                                 (" else " ++) . showsPrec 1 c2
  showsPrec p (Cwhile e c) =
    showParen (p > 1) $
    ("while " ++) . showsPrec 1 e . (" do " ++) . showsPrec 1 c

boolToString True = "true"
boolToString False = "false"

instance Show Data where
  showsPrec p (Data_Int n) = showsPrec p n
  showsPrec p (Data_Bool b) = (boolToString b ++)
  showsPrec p (Cell (c1, c2)) = showsPrec p c1 . (" : " ++) . showsPrec p c2

-- Parsing

isVar x = all isAlpha x && not (x `elem` keywords)
  where keywords = ["zero", "succ", "true", "not", "skip",
                    "for", "if", "then", "else", "while", "do",
                    "true", "false", "hd", "tl"]

when True p = p
when False _ = fail "when failed"

instance Read E where
  readPrec = parens $
             (prec 1 $ do
                e1 <- step readPrec
                Symbol ":" <- lexP
                e2 <- readPrec
                return (Econs e1 e2)) <++
             (prec 1 $ do
                e1 <- step readPrec
                Symbol "<" <- lexP
                e2 <- step readPrec
                return (Elt e1 e2)) <++
             (prec 1 $ do
                e1 <- step readPrec
                Symbol "=" <- lexP
                e2 <- step readPrec
                return (Eeq e1 e2)) <++
             (do
                Number n <- lexP
                when (numberToInteger n == Just 0) $ do
                  return Ezero) <++
             (prec 2 $ do
                Ident "succ" <- lexP
                e <- readPrec
                return (Esucc e)) <++
             (prec 2 $ do
                Ident "pred" <- lexP
                e <- readPrec
                return (Epred e)) <++
             (prec 0 $ do
                Ident "if" <- lexP
                e <- step readPrec
                Ident "then" <- lexP
                e1 <- step readPrec
                Ident "else" <- lexP
                e2 <- readPrec
                return (Eif e e1 e2)) <++
             (do
                Ident x <- lexP
                when (isVar x) $ do
                  return (Evar x)) <++
             (do
                Ident "true" <- lexP
                return Etrue) <++
             (do
                Ident "false" <- lexP
                return Efalse) <++
             (prec 2 $ do
                Ident "not" <- lexP
                e <- readPrec
                return (Enot e)) <++
             (prec 2 $ do
                Ident "hd" <- lexP
                e <- readPrec
                return (Ehd e)) <++
             (prec 2 $ do
                Ident "tl" <- lexP
                e <- readPrec
                return (Etl e))

instance Read C where
  readPrec = parens $
             (prec 0 $ do
                c1 <- step readPrec
                Punc ";" <- lexP
                c2 <- readPrec
                return (Cseq c1 c2)) <++
             (do
                Ident x <- lexP
                when (isVar x) $ do
                  Symbol ":=" <- lexP
                  e <- reset readPrec
                  return (Cassign x e)) <++
             (do
                Ident "skip" <- lexP
                return Cskip) <++
             (prec 1 $ do
                Ident "if" <- lexP
                e <- readPrec
                Ident "then" <- lexP
                c1 <- step readPrec
                Ident "else" <- lexP
                c2 <- readPrec
                return (Cif e c1 c2)) <++
             (prec 1 $ do
                Ident "for" <- lexP
                e <- readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cfor e c)) <++
             (prec 1 $ do
                Ident "while" <- lexP
                e <- readPrec
                Ident "do" <- lexP
                c <- readPrec
                return (Cwhile e c))

-- Main function: parsing a statement and pretty-printing

s0 (x, Null) = error ("not initialized variable " ++ x)

run c = print (semC c s0 ("result", Null))

main = do  input <- getContents
           let c :: C
               c = read input
           run c