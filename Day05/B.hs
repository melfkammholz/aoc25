module Day05.B where

import MyPrelude


type IngredientID = Int

data Database = Database [Range IngredientID] [IngredientID]
  deriving Show

databaseP :: Parser Database
databaseP = do
  rs <- some (rangeP numP <* newline)
  newline
  ids <- some (numP <* newline)
  return (Database rs ids)


fresh :: Database -> Int
fresh (Database (Sorted rs) _) = go rs
  where
    go [r] = size r
    go (r1@(Range a1 b1) : r2@(Range a2 b2) : rs)
      | a2 <= b1  = go (Range a1 (max b1 b2) : rs)
      | otherwise = size r1 + go (r2 : rs)


main :: IO ()
main = app databaseP (print . fresh)  -- 346240317247002

