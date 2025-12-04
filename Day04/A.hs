module Day04.A where

import MyPrelude
import Data.Array (bounds)


newtype Grid = Grid (Array (Int, Int) Char)
  deriving Show

gridP :: Parser Grid
gridP = do
  g <- some (some (oneOf ".@") <* newline)
  let (m, n) = (length g, length (g ! 0))
  return (Grid (listArray ((0, 0), (m - 1, n - 1)) (concat g)))


access :: Grid -> Int
access (Grid g) = lengthOn ((== '@') . (g !) .&& (< 4) . adj)
                           ([0..m] `cartesian` [0..n])
  where
    (_, (m, n)) = bounds g

    a !? p@(y, x) = if 0 <= y && y <= m && 0 <= x && x <= n
                      then a ! p
                      else '.'

    adj p@(y, x) = lengthOn ((/= p) .&& (== '@') . (g !?))
                            ([y - 1..y + 1] `cartesian` [x - 1..x + 1])


main :: IO ()
main = app gridP (print . access)  -- 1445

