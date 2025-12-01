{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Day01.B where

import Control.Monad.State.Lazy
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


password :: [Rot] -> Int
password rs = snd (execState (mapM (modify . rot) rs) (modInt @100 50, 0))
  where
    rot r@(Rot n) (x, p) = (x + modInt n, p + count (toInt x) r)

    count 0 (RotL n) = n `div` 100
    count x (RotL n) = (100 - x + n) `div` 100
    count x (RotR n) = (x + n) `div` 100


main :: IO ()
main = app rotsP (print . password)  -- 6558

