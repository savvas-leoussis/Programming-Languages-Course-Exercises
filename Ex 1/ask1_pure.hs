--Leousis Savvas
--A.M.:03114945

main = do  
  n <- readLn
  word <- getLine
  print (number_of_palindromes n word)

fillArray :: [Char] -> [Char] -> [[Integer]] -> [[Integer]]
fillArray word_a word_b (hd:[]) = []
fillArray word_a word_b lst = 
    if head word_a == head word_b then
        (((x+y+1):head(tail(lst))):(fillArray (tail word_a) (tail word_b) (tail lst)))
    else 
        (((x+y-z):head(tail(lst))):(fillArray (tail word_a) (tail word_b) (tail lst)))
    where 
        x = head (head lst)
        y = head (head (tail lst))
        z = head (tail (head lst))

nthTailOfList 0 l = l
nthTailOfList n l = nthTailOfList (n-1) (tail l)

listOfnLists10 :: Integer -> [[Integer]] -> [[Integer]]
listOfnLists10 0 l = l
listOfnLists10 n l = listOfnLists10 (n-1) ([1,0]:l)

solve :: [Char] -> [Char] -> [[Integer]] -> Integer -> Integer -> [[Integer]]
solve word word_b lst l n =
    if l <= n then
        solve word word_b' lst' (l+1) n
    else
        lst
    where
        k = l-1
        word_b' =
            if l == 2 then 
                nthTailOfList k word
            else
                tail word_b
        lst' = fillArray word word_b' lst

number_of_palindromes :: Integer -> [Char] -> Integer 
number_of_palindromes n word = answer `mod` 20130401
  where 
    lst = listOfnLists10 n []
    dpArray = solve word word lst 2 n
    answer = head (head dpArray)