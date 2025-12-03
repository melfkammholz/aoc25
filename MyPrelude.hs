{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module MyPrelude (
         app,
         lengthOn, foldl',

         module Control.Applicative,

         Parser, numP,

         sepBy, char, digit, newline,

         ModInt, modInt, toInt, moddiv, modinv,

         zalg,

         Container(..),
         Array, Array.listArray,
         fromList, toList
       ) where

import Control.Applicative
import Data.List (foldl')
import Data.Proxy
import Data.Kind
import qualified Data.Array as Array
import Data.Array (Array)
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Type.Bool
import Data.Type.Equality
import qualified GHC.IsList as IsList
import GHC.IsList (fromList, toList)
import GHC.TypeLits
import System.CPUTime
import Text.Parsec hiding (many)
import Text.Parsec.Char
import Text.Printf


app :: Parser a -> (a -> IO ()) -> IO ()
app p f = do
  inp <- getContents
  either print (time . f) (parse p "<stdin>" inp)

time :: IO a -> IO a
time act = do
  t1 <- getCPUTime
  !x <- act
  t2 <- getCPUTime
  printf "%.3fs\n" (fromIntegral (t2 - t1) / (10 ^ 12) :: Double)
  return x


lengthOn :: (a -> Bool) -> [a] -> Int
lengthOn p = foldl' (\l x -> l + if p x then 1 else 0) 0


type Parser = Parsec String ()

numP :: Parser Int
numP = read <$> many1 digit


type family NonZero (n :: Nat) (s :: ErrorMessage) :: Constraint where
  NonZero 0 s = TypeError s
  NonZero _ s = ()

type family PrimeHelper (n :: Nat) (p :: Nat) :: Bool where
  PrimeHelper n p =
    If (CmpNat n (p GHC.TypeLits.* p) == LT)
       'True
       (PrimeHelperContinue (Mod n p == 0) n (p + 1))

type family PrimeHelperContinue (b :: Bool) (n :: Nat) (p :: Nat) :: Bool where
  PrimeHelperContinue 'True  n p = 'False
  PrimeHelperContinue 'False n p = PrimeHelper n p

type family Prime (n :: Nat) :: Constraint where
  Prime n =
    If (PrimeHelper n 2)
       (() :: Constraint)
       (TypeError ('Text "Modulus " ':<>: ShowType n ':<>: 'Text " is not prime"))


data ModInt n = MI !Int
  deriving Eq

type family Modulus (m :: Nat) :: Constraint where
  Modulus m = (KnownNat m, NonZero m ('Text "Modulus cannot be zero."))

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

instance (Modulus m, Prime m) => Integral (ModInt m) where
  quotRem x y = (x * modinv y, modInt 0)
    where modinv x = x ^ (natVal (Proxy @m) - 2)
  toInteger (MI x) = toInteger x

modinv :: forall m. Modulus m => ModInt m -> ModInt m
modinv x = x ^ (tot (fromInteger (natVal (Proxy @m))) - 1)

moddiv :: Modulus m => ModInt m -> ModInt m -> ModInt m
moddiv x y = x * modinv y


zalg :: String -> Seq Int
zalg s = go 1 0 0 [n]
  where
    n = length s
    s' = fromList s :: Array Int Char

    match i c = if ok then 1 + match i (c + 1) else 0
      where ok = i + c < n && s' ! c == s' ! (i + c)

    go i _ _ z | i == n = z
    go i l r z = let c = if i < r then min (z ! (i - l)) (r - i) else 0
                     !zi = match i c
                     (l', r') = if i + zi > r then (i, i + zi) else (l, r)
                  in go (i + 1) l' r' (z |> zi)


class Container c where
  type Index c
  type Item c
  -- empty :: c
  singleton :: Item c -> c
  (!) :: c -> Index c -> Item c
  (<|) :: Item c -> c -> c
  (|>) :: c -> Item c -> c
  update :: Index c -> Item c -> c -> c
  -- (!?) :: c -> Index c -> Maybe (Item c)
  -- adjust :: Index c -> (Item c -> Item c) -> c -> c
  insert :: Index c -> Item c -> c -> c
  -- delete :: Index c -> c -> c

instance IsList.IsList (Array Int e) where
  type Item (Array _ e) = e
  toList = Array.elems
  fromList xs = Array.listArray (0, length xs - 1) xs

instance Container (Array Int e) where
  type Index _ = Int
  type Item (Array _ e) = e
  -- empty = error "not possible"
  singleton x = Array.listArray (0, 0) [x]
  (!) = (Array.!)
  x <| a = let (l, r) = Array.bounds a
            in Array.listArray (l - 1, r) (x : Array.elems a)
  a |> x = let (l, r) = Array.bounds a
            in Array.listArray (l, r + 1) (Array.elems a ++ [x])
  update k x a = a Array.// [(k, x)]
  insert k x a = let (l, r) = Array.bounds a
                     (xs, ys) = splitAt k (Array.elems a)
                  in Array.listArray (l, r + 1) (xs ++ [x] ++ ys)

instance {-# OVERLAPPABLE #-} Array.Ix i => Container (Array i e) where
  type Index (Array i _) = i
  type Item (Array _ e) = e

  singleton = undefined
  (<|) = undefined
  (|>) = undefined
  insert = undefined

  (!) = (Array.!)
  update k x a = a Array.// [(k, x)]

instance Container [a] where
  type Index _ = Int
  type Item [a] = a
  -- empty = []
  singleton x = [x]
  (!) = (!!)
  (<|) = (:)
  xs |> x = xs ++ [x]
  update 0 y (_ : xs) = y : xs
  update k y (x : xs) = x : update (k - 1) y xs
  insert 0 y xs = y : xs
  insert _ _ [] = error "empty list"
  insert k y (x : xs) = x : insert (k - 1) y xs

instance Container (Seq a) where
  type Index _ = Int
  type Item (Seq a) = a
  -- empty = []
  singleton x = [x]
  (!) = Seq.index
  (<|) = (Seq.<|)
  (|>) = (Seq.|>)
  update = Seq.update
  insert = Seq.insertAt

