{-# LANGUAGE DataKinds #-}
module Day01.A where

import Text.Parsec
import Text.Parsec.Char
import Utils (ModInt, modInt, toInt, Parser, numP)


data Rot = RotL Int | RotR Int
  deriving Show

rotP :: Parser Rot
rotP = (RotL <$> (char 'L' *> numP)) <|> (RotR <$> (char 'R' *> numP))

rotsP :: Parser [Rot]
rotsP = many (rotP <* newline)


dials :: [Rot] -> [ModInt 100]
dials rs = scanl (flip ($)) (modInt 50) (map rot rs)
  where
    rot (RotL n) x = x - modInt n
    rot (RotR n) x = x + modInt n

password :: [Rot] -> Int
password = length . filter (== modInt 0) . dials


main :: IO ()
main = interact $ \s -> case parse rotsP "" s of
                          Left err -> show err
                          Right rots -> show (password rots)  -- 1135

