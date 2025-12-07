module Day04.A where

import MyPrelude


access :: Grid Char -> Int
access g = lengthOn ((== '@') . (g !) .&& (< 4) . adj)
                    ([0..m] `cartesian` [0..n])
  where
    (_, (m, n)) = bounds g

    a !? p@(y, x) = if 0 <= y && y <= m && 0 <= x && x <= n
                      then a ! p
                      else '.'

    adj p@(y, x) = lengthOn ((/= p) .&& (== '@') . (g !?))
                            ([y - 1..y + 1] `cartesian` [x - 1..x + 1])


main :: IO ()
main = app (gridP (oneOf ".@")) (print . access)  -- 1445

