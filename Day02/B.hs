module Day02.B where

import MyPrelude
import qualified Data.Vector.Unboxed as VU


type ProductID = Int

rangesP :: Parser [Range ProductID]
rangesP = (rangeP numP `sepBy` char ',') <* newline


isRep :: String -> Bool
isRep s =
  let n = length s
      z = zalg s
   in VU.ifoldr' (\i zi b -> b || i + zi == n && gcd i zi == i) False z

silly :: Range ProductID -> [ProductID]
silly (Range a b) = filter (isRep . show) [a..b]


main :: IO ()
main = app rangesP (print . sum . concatMap silly)  -- 45814076230

