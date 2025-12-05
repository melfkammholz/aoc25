module Day02.A where

import MyPrelude
import Data.List (inits, tails)


type ProductID = Int

rangesP :: Parser [Range ProductID]
rangesP = (rangeP numP `sepBy` char ',') <* newline


invalid :: ProductID -> Bool
invalid x = or (zipWith (==) (inits (show x)) (tails (show x)))

silly :: Range ProductID -> [ProductID]
silly (Range a b) = filter invalid [a..b]


main :: IO ()
main = app rangesP (print . sum . concatMap silly)  -- 35367539282

