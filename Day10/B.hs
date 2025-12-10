{-# LANGUAGE DeriveGeneric #-}
module Day10.B where

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
minPresses (Machine _ sems req) = go 0 (fromList [req']) Seq.empty HashSet.empty
  where
    req' = Seq.fromList req

    go d Seq.Empty      next seen = go (d + 1) next Seq.empty seen
    go d (l Seq.:<| ls) next seen
      | l `isElem` seen || not (feasible l) = go d ls next seen
      | all (== 0) l    = d
      | otherwise       = go d ls (next Seq.>< fromList (map (toggle l) sems)) (insert l seen)

    toggle l = foldr (\b l' -> Seq.adjust (-1) b l') l

    feasible = all (<= 0)

main :: IO ()
main = app (many manP) (print . sum . map minPresses)  -- X

