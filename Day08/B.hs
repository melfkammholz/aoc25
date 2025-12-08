module Day08.B where

import MyPrelude
import qualified Utils.UnionFind as UF
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.List (tails)


vec3P :: Parser (Vec3 Int)
vec3P = Vec3 <$> (numP <* char ',') <*> (numP <* char ',') <*> (numP <* newline)

vec3sP :: Parser [Vec3 Int]
vec3sP = many vec3P


solve :: [Vec3 Int] -> Int
solve vs = runST $ do
    uf <- UF.new (length vs)
    (v, w) <- mst uf es
    return (vec3X (vs ! v) * vec3X (vs ! w))
  where
    es = map idxs (sortOn key [(v, w) | (v : ws) <- tails (zip [0..] vs), w <- ws])
      where
        idxs ((v, _), (w, _)) = (v, w)
        key ((_, v), (_, w)) = dist2sq v w

    mst uf ((v, w) : es) = do
      ing <- UF.same v w uf
      when (not ing) (UF.union v w uf)
      n <- UF.numClasses uf
      if n == 1 then return (v, w) else mst uf es


main :: IO ()
main = app vec3sP (print . solve)  -- 673096646

