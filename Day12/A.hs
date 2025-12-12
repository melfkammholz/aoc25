module Day12.A where

import MyPrelude
import Control.Monad


type Situation = ([Grid Char], [(Int, Int, [Int])])

shapeP :: Parser (Grid Char)
shapeP = do
  numP *> char ':' *> newline
  gridP (oneOf ".#")

regionP :: Parser (Int, Int, [Int])
regionP = do
  w <- numP
  char 'x'
  h <- numP
  string ": "
  ns <- numP `sepBy` spaces
  return (w, h, ns)

situationP :: Parser Situation
situationP = do
  gs <- replicateM 6 (shapeP <* newline)
  rs <- many (regionP <* newline)
  return (gs, rs)


solve :: Situation -> Int
solve (gs, rs) = lengthOn id (map fits rs)
  where
    as = map (lengthOn (== '#')) gs
    req = sum . zipWith (*) as
    fits (w, h, ns) = req ns <= w * h


main :: IO ()
main = app situationP (print . solve)  -- 495

