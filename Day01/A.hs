{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Day01.A where

import MyPrelude


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
password = lengthOn (== 0) . scanl (\x (Rot n) -> x + modInt n) (modInt @100 50)


main :: IO ()
main = app rotsP (print . password)  -- 1135

