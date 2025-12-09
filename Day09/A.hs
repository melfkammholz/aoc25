module Day09.A where

import MyPrelude
import Data.List (tails)


tilesP :: Parser [(Int, Int)]
tilesP = many ((,) <$> (numP <* char ',') <*> (numP <* newline))

maxRect :: [(Int, Int)] -> Int
maxRect ts =
  maximum [dy * dx | ((y1, x1) : ts') <- tails ts
                   , (y2, x2) <- ts'
                   , let dy = max y1 y2 - min y1 y2 + 1
                   , let dx = max x1 x2 - min x1 x2 + 1
                   ]

main :: IO ()
main = app tilesP (print . maxRect)  -- 4777967538

