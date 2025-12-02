module Day02.A where

import MyPrelude
import Data.List (inits, tails)


data Range = Range Int Int
  deriving Show

rangeP :: Parser Range
rangeP = Range <$> numP <*> (char '-' *> numP)

rangesP :: Parser [Range]
rangesP = (rangeP `sepBy` char ',') <* newline


invalid :: Int -> Bool
invalid x = or (zipWith (==) (inits (show x)) (tails (show x)))

silly :: Range -> [Int]
silly (Range a b) = filter invalid [a..b]


main :: IO ()
main = app rangesP (print . sum . concatMap silly)

