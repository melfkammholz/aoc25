module Day04.A where

import MyPrelude
import qualified Data.Array as Array

newtype Grid = Grid (Array (Int, Int) Char)
  deriving Show

gridP :: Parser Grid
gridP = do
  g <- some (some (oneOf ".@") <* newline)
  let (m, n) = (length g, length (g ! 0))
  return (Grid (listArray ((0, 0), (m - 1, n - 1)) (concat g)))

isRoll :: Char -> Bool
isRoll = (== '@')

access :: Grid -> Int
access (Grid g) = sum [1 | y <- [0..m]
                         , x <- [0..n]
                         , isRoll (g ! (y, x))
                         , adj y x < 4
                         ]
  where
    (_, (m, n)) = Array.bounds g
    inBounds (i, j) = 0 <= i && i <= m && 0 <= j && j <= n
    a !? k | inBounds k = Just (a ! k)
           | otherwise = Nothing

    adj y x = sum [1 | i <- [y - 1..y + 1]
                     , j <- [x - 1..x + 1]
                     , (i, j) /= (y, x)
                     , maybe False isRoll (g !? (i, j))
                     ]

main :: IO ()
main = app gridP (print . access)  -- 1445

