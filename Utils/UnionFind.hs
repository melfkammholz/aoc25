module Utils.UnionFind where

import Control.Monad (when)
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as VU

data UnionFind s = UF (STRef s Int) (VU.MVector s Int)

new :: Int -> ST s (UnionFind s)
new n = do
  nref <- newSTRef n
  v <- VU.replicate n (-1)
  return (UF nref v)

find :: Int -> UnionFind s -> ST s Int
find x uf@(UF _ v) = do
  y <- VU.read v x
  if y < 0
    then return x
    else do
      r <- find y uf
      VU.write v x r
      return r

union :: Int -> Int -> UnionFind s -> ST s ()
union x y uf@(UF nref v) = do
  rx <- find x uf
  ry <- find y uf
  when (rx /= ry) $ do
    sx <- VU.read v rx
    sy <- VU.read v ry
    let (x', y') = if sx < sy then (rx, ry) else (ry, rx)
    VU.write v x' (sx + sy)
    VU.write v y' x'
    modifySTRef nref pred

same :: Int -> Int -> UnionFind s -> ST s Bool
same x y uf = (==) <$> find x uf <*> find y uf

sizes :: UnionFind s -> ST s [Int]
sizes (UF _ v) = VU.foldr' (\v vs -> if v < 0 then (-v) : vs else vs) [] v

numClasses :: UnionFind s -> ST s Int
numClasses (UF nref _) = readSTRef nref

