{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Day01.B where

import Data.List (foldl')
import Text.Parsec
import Text.Parsec.Char
import Utils


data Rot = RotL Int | RotR Int
  deriving Show

rotP :: Parser Rot
rotP = RotL <$> (char 'L' *> numP) <|> RotR <$> (char 'R' *> numP)

rotsP :: Parser [Rot]
rotsP = many (rotP <* newline)

dist :: Rot -> Int
dist (RotL n) = -n
dist (RotR n) = n

pattern Rot :: Int -> Rot
pattern Rot n <- (dist -> n)


password :: [Rot] -> Int
password = snd . foldl' rot (modInt @100 50, 0)
  where
    rot (x, p) r@(Rot n) = (x + modInt n, p + count (toInt x) r)

    count 0 (RotL n) = n `div` 100
    count x (RotL n) = (100 - x + n) `div` 100
    count x (RotR n) = (x + n) `div` 100


main :: IO ()
main = app rotsP (print . password)  -- 6558

