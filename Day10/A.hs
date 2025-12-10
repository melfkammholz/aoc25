{-# LANGUAGE DeriveGeneric #-}
module Day10.A where

import MyPrelude
import GHC.Generics (Generic)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet


data Machine = Machine String [[Int]] [Int]
  deriving (Eq, Generic, Show)

instance Hashable Machine

manP :: Parser Machine
manP = do
  let diagP = between (char '[') (char ']') (many (oneOf ".#"))
      semP  = between (char '(') (char ')') (numP `sepBy` char ',')
      reqP  = between (char '{') (char '}') (numP `sepBy` char ',')
  diag <- diagP <* spaces
  sems <- many (semP <* spaces)
  req <- reqP <* newline
  return (Machine diag sems req)

minPresses :: Machine -> Int
minPresses (Machine diag sems _) = go 0 (fromList [Seq.replicate (length diag) False]) Seq.empty HashSet.empty
  where
    diag' = Seq.fromList (map (== '#') diag)

    go d Seq.Empty      next seen = go (d + 1) next Seq.empty seen
    go d (l Seq.:<| ls) next seen
      | l `isElem` seen = go d ls next seen
      | diag' == l      = d
      | otherwise       = go d ls (next Seq.>< fromList (map (toggle l) sems)) (insert l seen)

    toggle = foldr (\b l' -> Seq.adjust not b l')

main :: IO ()
main = app (many manP) (print . sum . map minPresses)  -- X

