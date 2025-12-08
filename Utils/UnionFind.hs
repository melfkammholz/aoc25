module Utils.UnionFind where

import MyPrelude hiding (find)
import qualified Data.Sequence as Seq

type UnionFind = Seq Int

new :: Int -> UnionFind
new n = Seq.replicate n (-1)

sizes :: UnionFind -> [Int]
sizes = foldr (\v vs -> if v < 0 then (-v) : vs else vs) []

find :: Int -> UnionFind -> (Int, UnionFind)
find v uf
  | uf ! v < 0 = (v, uf)
  | otherwise  = let (p, uf') = find (uf ! v) uf in (p, updateAt v p uf')

union :: Int -> Int -> UnionFind -> UnionFind
union v w uf0 = uf4
  where
    (pv, uf1) = find v uf0
    (pw, uf2) = find w uf1
    (pv', pw') = if uf2 ! pv < uf2 ! pw then (pv, pw) else (pw, pv)
    uf3 = updateAt pv' (uf2 ! pv' + uf2 ! pw') uf2
    uf4 = updateAt pw' pv' uf3

same :: Int -> Int -> UnionFind -> (Bool, UnionFind)
same v w uf = (pv == pw, uf'')
  where
    (pv, uf')  = find v uf
    (pw, uf'') = find w uf'

