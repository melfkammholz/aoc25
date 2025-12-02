module Day02.B where

import MyPrelude
import qualified Data.Sequence as Seq

data Range = Range Int Int
  deriving Show

rangeP :: Parser Range
rangeP = Range <$> numP <*> (char '-' *> numP)

rangesP :: Parser [Range]
rangesP = (rangeP `sepBy` char ',') <* newline


isRep :: String -> Bool
isRep s =
  let n = length s
      z = zalg s
   in Seq.foldrWithIndex (\i zi b -> b || i + zi == n && gcd i zi == i) False z

silly :: Range -> [Int]
silly (Range a b) = filter (isRep . show) [a..b]


main :: IO ()
main = app rangesP (print . sum . concatMap silly)  -- 45814076230

