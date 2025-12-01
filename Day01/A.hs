{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Day01.A where

import Text.Parsec
import Text.Parsec.Char
import Utils


data Rot = RotL Int | RotR Int
  deriving Show

rotP :: Parser Rot
rotP = RotL <$> (char 'L' *> numP) <|> RotR <$> (char 'R' *> numP)

rotsP :: Parser [Rot]
rotsP = many (rotP <* newline)

distance :: Rot -> Int
distance (RotL n) = -n
distance (RotR n) = n

pattern Rot :: Int -> Rot
pattern Rot n <- (distance -> n)


dials :: [Rot] -> [ModInt 100]
dials rs = scanl (flip ($)) (modInt 50) (map rot rs)
  where rot (Rot n) x = x + modInt n

password :: [Rot] -> Int
password = length . filter (== modInt 0) . dials


main :: IO ()
main = app rotsP (print . password)  -- 1135

