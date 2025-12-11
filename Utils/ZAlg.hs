module Utils.ZAlg where

import Control.Monad (when)
import Control.Monad.ST
import Data.Array (Array, (!), listArray)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f !x | p x       = while p f (f x)
             | otherwise = x

{-# SPECIALIZE zalg :: String -> VU.Vector Int #-}
zalg :: Eq a => [a] -> VU.Vector Int
zalg s = runST $ do 
    z <- MVU.new n
    MVU.unsafeWrite z 0 n
    go 1 0 0 z 
    VU.freeze z
  where
    n = length s
    s' = listArray (0, n - 1) s

    go i l r z = when (i < n) $ do
      when (i < r) $ do
        zj <- MVU.unsafeRead z (i - l)
        MVU.unsafeWrite z i (min zj (r - i))
      let ok i j = i + j < n && s' ! j == s' ! (i + j)
      zi <- while (ok i) (+1) <$> MVU.unsafeRead z i
      MVU.unsafeWrite z i zi
      let (l', r') = if i + zi > r then (i, i + zi) else (l, r)
      go (i + 1) l' r' z 
