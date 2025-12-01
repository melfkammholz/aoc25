{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Day01.B where

import Control.Monad.State.Lazy
import Data.Proxy
import Text.Parsec
import Text.Parsec.Char
import GHC.TypeLits


type Parser = Parsec String ()


data ModInt n = MI !Int
  deriving Eq

instance KnownNat n => Show (ModInt n) where
  show (MI x) = show x ++ " (mod " ++ show (natVal (Proxy @n)) ++ ")"

smod :: Integral a => a -> a -> a
smod x m = (x `mod` m + m) `mod` m

modInt :: forall n. KnownNat n => Int -> ModInt n
modInt x = MI (x `smod` fromInteger (natVal (Proxy @n)))

toInt :: ModInt n -> Int
toInt (MI x) = x

instance KnownNat n => Num (ModInt n) where
  MI x + MI y = modInt (x + y)
  MI x - MI y = modInt (x - y)
  MI x * MI y = modInt (x * y)
  abs (MI x) = MI x
  signum (MI 0) = 0
  signum (MI _) = 1
  fromInteger x = modInt (fromInteger x)


data Rot = RotL Int | RotR Int
  deriving Show

numP :: Parser Int
numP = read <$> many1 digit

rotP :: Parser Rot
rotP = (RotL <$> (char 'L' *> numP)) <|> (RotR <$> (char 'R' *> numP))

rotsP :: Parser [Rot]
rotsP = many (rotP <* newline)


password :: [Rot] -> Int
password rs = snd (execState (mapM (modify . uncurry . rot) rs) (modInt @100 50, 0))
  where
    rot r@(RotL n) x p = (x - modInt n, p + count (toInt x) r)
    rot r@(RotR n) x p = (x + modInt n, p + count (toInt x) r)

    count 0 (RotL n) = n `div` 100
    count x (RotL n) = (100 - x + n) `div` 100
    count x (RotR n) = (x + n) `div` 100


main :: IO ()
main = interact $ \s -> case parse rotsP "" s of
                          Left err -> show err
                          Right rots -> show (password rots)  -- 6558

