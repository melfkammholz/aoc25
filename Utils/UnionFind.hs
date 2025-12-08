module Utils.UnionFind where

import Control.Monad (when)
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as VU

type UnionFind s = VU.MVector s Int

new :: Int -> ST s (UnionFind s)
new n = VU.replicate n (-1)

find :: Int -> UnionFind s -> ST s Int
find x uf = do
  y <- VU.read uf x
  if y < 0
    then return x
    else do
      r <- find y uf
      VU.write uf x r
      return r

union :: Int -> Int -> UnionFind s -> ST s ()
union x y uf = do
  rx <- find x uf
  ry <- find y uf
  when (rx /= ry) $ do
    sx <- VU.read uf rx
    sy <- VU.read uf ry
    let (x', y') = if sx < sy then (rx, ry) else (ry, rx)
    VU.write uf x' (sx + sy)
    VU.write uf y' x'

same :: Int -> Int -> UnionFind s -> ST s Bool
same x y uf = (==) <$> find x uf <*> find y uf

sizes :: UnionFind s -> ST s [Int]
sizes = VU.foldr (\v vs -> if v < 0 then (-v) : vs else vs) []
