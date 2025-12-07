{-# LANGUAGE OverloadedLists #-}
module Day04.C where

import MyPrelude hiding (Grid, gridP)


newtype Grid = Grid (HashSet (Int, Int))
  deriving Show

gridP :: Parser Grid
gridP = do
  g <- some (some (oneOf ".@") <* newline)
  let s = fromList [(y, x) | (y, r) <- zip [0..] g
                           , (x, '@') <- zip [0..] r
                           ]
  return (Grid s)


access :: Grid -> Int
access (Grid g) = go (toList g) g [] ([] :: HashSet (Int, Int)) 0
  where
    go []      _ d [] !r = r
    go []      s d u  !r = go (toList (foldr delete u d))
                              (foldr delete s d)
                              []
                              []
                              (length d + r)
    go (p : q) s d u  !r
      | length ps < 4 = go q s (p : d) (foldr insert u ps) r
      | otherwise     = go q s d u r
      where ps = adj p s

    adj p@(y, x) s = [(y + dy, x + dx) | dy <- [-1..1]
                                       , dx <- [-1..1]
                                       , (dy, dx) /= (0, 0)
                                       , let p = (y + dy, x + dx)
                                       , p `isElem` s
                                       ]


main :: IO ()
main = app gridP (print . access)  -- 8317

