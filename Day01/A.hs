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

dist :: Rot -> Int
dist (RotL n) = -n
dist (RotR n) = n

pattern Rot :: Int -> Rot
pattern Rot n <- (dist -> n)


dials :: [Rot] -> [ModInt 100]
dials = scanl (\x (Rot n) -> x + modInt n) (modInt 50)

password :: [Rot] -> Int
password = length . filter (== 0) . dials


main :: IO ()
main = app rotsP (print . password)  -- 1135

