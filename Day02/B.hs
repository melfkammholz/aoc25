module Day02.B where

import MyPrelude
import qualified Data.Vector.Unboxed as VU


type ProductID = Int

rangesP :: Parser [Range ProductID]
rangesP = (rangeP numP `sepBy` char ',') <* newline


isRep :: String -> Bool
isRep s = VU.ifoldr' (\i zi -> (|| i + zi == n && gcd i zi == i)) False (zalg s)
  where n = length s

silly :: Range ProductID -> [ProductID]
silly (Range a b) = filter (isRep . show) [a..b]


main :: IO ()
main = app rangesP (print . sum . concatMap silly)  -- 45814076230

