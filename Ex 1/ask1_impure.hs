--Leousis Savvas
--A.M.:03114945

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.ST

main = do
  n <- readLn
  word_list <- getLine
  print $ number_of_palindromes n word_list

--get the (0,n-1)th element of the dp array
number_of_palindromes :: Int -> [Char] -> Int
number_of_palindromes n word_list = loop n word_list ! (0,n-1)

loop :: Int -> [Char] -> UArray (Int, Int) Int
loop n word_list = runSTUArray $ do
  dp <- newArray ((0,0),(n-1,n-1)) 1 -- create a dp n x n array initialized to 1
  word_array <- newListArray (0,n-1) word_list :: ST s (STUArray s Int Char) -- convert the input word from list to array
  forM_ [0..n-2] $ \i -> do
    a <- readArray word_array i
    b <- readArray word_array (i+1)
    if (a == b) then
      writeArray dp (i,i+1) 3
    else
      writeArray dp (i,i+1) 2
  forM_ [3..n] $ \l -> do
    forM_ [0..n-l] $ \i -> do
      let j = l+i-1
      x <- readArray dp (i,j-1)
      y <- readArray dp (i+1,j)
      z <- readArray dp (i+1,j-1)
      a <- readArray word_array i
      b <- readArray word_array j
      if (a == b) then
        writeArray dp (i,j) $ mod (x+y+1) 20130401
      else
        writeArray dp (i,j) $ mod (x+y-z) 20130401
  return dp 