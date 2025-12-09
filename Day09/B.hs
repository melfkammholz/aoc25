module Day09.B where

import MyPrelude
import Data.List (tails)


type Point = (Int, Int)

tilesP :: Parser [Point]
tilesP = many ((,) <$> (numP <* char ',') <*> (numP <* newline))


maxRect :: [Point] -> Int
maxRect ts = maximum [area p1 p2 | (p1 : ts') <- tails ts, p2 <- ts'
                                 , not (bad ts (p1, p2))
                                 ]
  where
    dist (a, b) = b - a + 1
    area (y1, x1) (y2, x2) = dist (minmax y1 y2) * dist (minmax x1 x2)

    bad ts ((y1, x1), (y2, x2)) = any inter (zip ts (tail ts ++ [head ts]))
      where
        (ymin, ymax) = minmax y1 y2
        (xmin, xmax) = minmax x1 x2
        inter ((y1, x1), (y2, x2)) =
          xmin < max x1 x2 && min x1 x2 < xmax
            && ymin < max y1 y2 && min y1 y2 < ymax


main :: IO ()
main = app tilesP (print . maxRect)  -- 1439894345

