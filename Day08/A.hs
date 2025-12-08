module Day08.A where

import MyPrelude
import qualified Utils.UnionFind as UF
import Control.Monad.ST (runST)
import Data.List (tails)


vec3P :: Parser (Vec3 Int)
vec3P = do
  [x, y, z] <- (numP `sepBy` char ',') <* newline
  return (Vec3 x y z)

vec3sP :: Parser [Vec3 Int]
vec3sP = many vec3P


solve :: Int -> [Vec3 Int] -> Int
solve k vs = runST $ do
    uf <- UF.new (length vs)
    mst uf k es
    ss <- UF.sizes uf
    return (product . take 3 . sortOn negate $ ss)
  where
    es = map idxs (sortOn key [(v, w) | (v : ws) <- tails (zip [0..] vs), w <- ws])
      where
        idxs ((v, _), (w, _)) = (v, w)
        key ((_, v), (_, w)) = dist2sq v w

    mst uf 0 _           = return ()
    mst uf k ((v, w):es) = do
      ing <- UF.same v w uf
      if ing
        then mst uf (k - 1) es
        else do
          UF.union v w uf
          mst uf (k - 1) es


main :: IO ()
main = app vec3sP (print . solve 1000)  -- 123420

