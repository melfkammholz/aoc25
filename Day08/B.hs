module Day08.B where

import MyPrelude
import qualified Utils.UnionFind as UF
import Control.Monad.ST (runST)
import Data.List (tails, findIndex)


data Vec3 a = Vec3 a a a
  deriving Show

vec3P :: Parser (Vec3 Int)
vec3P = do
  [x, y, z] <- (numP `sepBy` char ',') <* newline
  return (Vec3 x y z)

vec3sP :: Parser [Vec3 Int]
vec3sP = many vec3P


dist2 :: Num a => Vec3 a -> Vec3 a -> a
dist2 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2

getX :: Vec3 a -> a
getX (Vec3 x _ _) = x

solve :: [Vec3 Int] -> Int
solve vs = runST $ do
    uf <- UF.new (length vs)
    (v, w) <- mst uf es
    return (getX (vs ! v) * getX (vs ! w))
  where
    es = map idxs (sortOn key [(v, w) | (v : ws) <- tails (zip [0..] vs), w <- ws])
      where
        idxs ((v, _), (w, _)) = (v, w)
        key ((_, v), (_, w)) = dist2 v w

    mst uf ((v, w) : es) = do
      ing <- UF.same v w uf
      if ing
        then mst uf es
        else do
          UF.union v w uf
          n <- UF.numClasses uf
          if n == 1
            then return (v, w)
            else mst uf es


main :: IO ()
main = app vec3sP (print . solve)  -- 673096646

