module Day04.B where

import MyPrelude
import Data.Array (Array, indices)


newtype Grid = Grid Graph
  deriving Show

gridP :: Parser Grid
gridP = do
  g <- some (some (oneOf ".@") <* newline)

  let (m, n) = (length g - 1, length (g ! 0) - 1)
      s = listArray ((0, 0), (m, n)) (concat g)

      adj p@(y, x) =
        [y' * (n + 1) + x' | (dy, dx) <- [-1..1] `cartesian` [-1..1]
                           , let p'@(y', x') = (y + dy, x + dx)
                           , 0 <= y' && y' <= m && 0 <= x' && x' <= n
                           , p' /= p
                           , s ! p' == '@'
                           ]

      (gr, _, _) =
        graphFromEdges
          [((), y * (n + 1) + x, adj p) | p@(y, x) <- [0..m] `cartesian` [0..n]
                                        , s ! p == '@'
                                        ]

  return (Grid gr)


access :: Grid -> Int
access (Grid g) = if null vs then 0 else length vs + access (Grid g')
  where
    ind = indegree g
    vs = fromList (filter (\v -> ind ! v < 4) (indices g))
    g' = deleteVertices vs g


main :: IO ()
main = app gridP (print . access)  -- 8317
