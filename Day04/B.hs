module Day04.B where

import MyPrelude


toGraph :: Grid Char -> Graph
toGraph g =
  let (_, (m, n)) = bounds g
      adj p@(y, x) =
        [y' * (n + 1) + x' | (dy, dx) <- [-1..1] `cartesian` [-1..1]
                           , let p'@(y', x') = (y + dy, x + dx)
                           , 0 <= y' && y' <= m && 0 <= x' && x' <= n
                           , p' /= p
                           , g ! p' == '@'
                           ]
      (gr, _, _) =
        graphFromEdges
          [((), y * (n + 1) + x, adj p) | p@(y, x) <- [0..m] `cartesian` [0..n]
                                        , g ! p == '@'
                                        ]
   in gr

access :: Graph -> Int
access g = m + if m > 0 then access g' else 0
  where
    vs = fmap (< 4) (indegree g)
    m = lengthOn id vs
    g' = deleteVertices vs g


main :: IO ()
main = app (gridP (oneOf ".@")) (print . access . toGraph)  -- 8317
