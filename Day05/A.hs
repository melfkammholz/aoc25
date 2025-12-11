module Day05.A where

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
fresh (Database (Sorted rs) (Sorted ids)) = go rs ids
  where
    go [] _  = 0
    go _  [] = 0
    go (r@(Range a b) : rs) (id : ids)
      | id < a    = go (r : rs) ids
      | id > b    = go rs (id : ids)
      | otherwise = 1 + go (r : rs) ids


main :: IO ()
main = app databaseP (print . fresh)  -- 896

