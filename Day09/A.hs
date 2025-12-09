module Day09.A where

import MyPrelude
import Data.List (tails)


type Point = (Int, Int)

tilesP :: Parser [Point]
tilesP = many ((,) <$> (numP <* char ',') <*> (numP <* newline))


maxRect :: [Point] -> Int
maxRect ts = maximum [area p1 p2 | (p1 : ts') <- tails ts, p2 <- ts']
  where
    dist (a, b) = b - a + 1
    area (y1, x1) (y2, x2) = dist (minmax y1 y2) * dist (minmax x1 x2)


main :: IO ()
main = app tilesP (print . maxRect)  -- 4777967538

