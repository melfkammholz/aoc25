{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Utils (
         app,
         ModInt, modInt, toInt,
         Parser, numP
       ) where

import Data.Proxy
import Data.Kind
import GHC.TypeLits
import Text.Parsec


app :: Parser a -> (a -> IO ()) -> IO ()
app p f = do
  inp <- getContents
  either print f (parse p "<stdin>" inp)


type Parser = Parsec String ()

numP :: Parser Int
numP = read <$> many1 digit


type family NonZero (n :: Nat) (s :: ErrorMessage) :: Constraint where
  NonZero 0 s = TypeError s
  NonZero _ s = ()

type (a :: Constraint) && (b :: Constraint) = (a, b)


data ModInt n = MI !Int
  deriving Eq

type family Modulus (m :: Nat) :: Constraint where
  Modulus m = KnownNat m && NonZero m ('Text "Modulus cannot be zero.")

instance KnownNat m => Show (ModInt m) where
  show (MI x) = show x ++ " (mod " ++ show (natVal (Proxy @m)) ++ ")"

smod :: Integral a => a -> a -> a
smod x m = (x `mod` m + m) `mod` m

instance Modulus m => Num (ModInt m) where
  MI x + MI y = modInt (x + y)
  MI x - MI y = modInt (x - y)
  MI x * MI y = modInt (x * y)
  abs (MI x) = MI x
  signum (MI 0) = 0
  signum (MI _) = 1
  fromInteger x = MI (fromInteger x `smod` fromInteger (natVal (Proxy @m)))

instance Modulus m => Ord (ModInt m) where
  MI x <= MI y = x <= y

instance Modulus m => Real (ModInt m) where
  toRational (MI x) = toRational x

modInt :: forall m. Modulus m => Int -> ModInt m
modInt x = MI (x `smod` fromInteger (natVal (Proxy @m)))

toInt :: ModInt m -> Int
toInt (MI x) = x

instance Modulus m => Enum (ModInt m) where
  toEnum = modInt
  fromEnum = toInt

pf :: Integral a => a -> [a]
pf = go 2
  where
    go _ 1             = []
    go p x | x < p * p = [x]
           | r == 0    = p : go p q
           | otherwise = go (p + 1) x
      where (q, r) = quotRem x p

tot :: Integral a => a -> a
tot = go 1 . pf
  where
    go r  [p]                  = p * r - r
    go !r (p:q:ps) | p == q    = go (p * r) (q:ps)
                   | otherwise = (p * r - r) * go 1 (q:ps)


instance Modulus m => Integral (ModInt m) where
  quotRem x y = (x * modinv y, modInt 0)
    where modinv x = x ^ (natVal (Proxy @m) - 2)
    -- where modinv x = x ^ (tot (fromInteger (natVal (Proxy @m))) - 1)
  toInteger (MI x) = toInteger x

