module Utils (
         ModInt, modInt, toInt,
         Parser, numP
       ) where

import Data.Proxy
import GHC.TypeLits
import Text.Parsec


type Parser = Parsec String ()

numP :: Parser Int
numP = read <$> many1 digit


data ModInt n = MI !Int
  deriving Eq

instance KnownNat n => Show (ModInt n) where
  show (MI x) = show x ++ " (mod " ++ show (natVal (Proxy @n)) ++ ")"

smod :: Integral a => a -> a -> a
smod x m = (x `mod` m + m) `mod` m

instance KnownNat n => Num (ModInt n) where
  MI x + MI y = modInt (x + y)
  MI x - MI y = modInt (x - y)
  MI x * MI y = modInt (x * y)
  abs (MI x) = MI x
  signum (MI 0) = 0
  signum (MI _) = 1
  fromInteger x = MI (fromInteger x `smod` fromInteger (natVal (Proxy @n)))

instance KnownNat n => Ord (ModInt n) where
  MI x <= MI y = x <= y

instance KnownNat n => Real (ModInt n) where
  toRational (MI x) = toRational x

modInt :: forall n. KnownNat n => Int -> ModInt n
modInt x = MI (x `smod` fromInteger (natVal (Proxy @n)))

toInt :: ModInt n -> Int
toInt (MI x) = x

instance KnownNat n => Enum (ModInt n) where
  toEnum = modInt
  fromEnum = toInt

instance KnownNat n => Integral (ModInt n) where
  quotRem x y = (x * modinv y, modInt 0)
    where modinv x = x ^ (natVal (Proxy @n) - 2)
  toInteger (MI x) = toInteger x

