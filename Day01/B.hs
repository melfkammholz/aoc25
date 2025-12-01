{-# LANGUAGE DataKinds #-}
module Day01.B where

import Control.Monad.State.Lazy
import Text.Parsec
import Text.Parsec.Char
import Utils (modInt, toInt, Parser, numP)


data Rot = RotL Int | RotR Int
  deriving Show

rotP :: Parser Rot
rotP = (RotL <$> (char 'L' *> numP)) <|> (RotR <$> (char 'R' *> numP))

rotsP :: Parser [Rot]
rotsP = many (rotP <* newline)


password :: [Rot] -> Int
password rs = snd (execState (mapM (modify . rot) rs) (modInt @100 50, 0))
  where
    rot r@(RotL n) (x, p) = (x - modInt n, p + count (toInt x) r)
    rot r@(RotR n) (x, p) = (x + modInt n, p + count (toInt x) r)

    count 0 (RotL n) = n `div` 100
    count x (RotL n) = (100 - x + n) `div` 100
    count x (RotR n) = (x + n) `div` 100


main :: IO ()
main = interact $ \s -> case parse rotsP "" s of
                          Left err -> show err
                          Right rots -> show (password rots)  -- 6558

