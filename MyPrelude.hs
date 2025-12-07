{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module MyPrelude (
         app,
         hornerL, hornerR,
         cartesian, chunksOf, lengthOn, find, foldl', sort, transpose, unsnoc,

         module Control.Applicative,

         Parser, numP, spaces, Grid, gridP,

         sepBy, oneOf, char, digit, newline,

         ModInt, modInt, toInt, moddiv, modinv,

         zalg,

         Container(..),
         UnorderedContainer(..),
         Array, Array.bounds, Array.listArray,
         HashSet,
         fromList, toList,
         Range(..), rangeP,

         BoolAlgebra(..),

         Graph, Graph.graphFromEdges, Graph.indegree,
         deleteVertices
       ) where

import Control.Applicative
import Data.List (find, foldl', sort, transpose)
import Data.Proxy
import Data.Kind
import Data.Foldable (Foldable)
import qualified Data.Array as Array
import Data.Array (Array)
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Type.Bool
import Data.Type.Equality
import qualified GHC.IsList as IsList
import GHC.IsList (fromList, toList)
import GHC.TypeLits
import System.CPUTime
import Text.Parsec hiding (many, newline, spaces)
import Text.Parsec.Char (char)
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


hornerL :: (Foldable t, Num a) => t a -> a
hornerL = foldl' (\x d -> x * 10 + d) 0

hornerR :: (Foldable t, Num a) => t a -> a
hornerR = foldr (\d x -> x * 10 + d) 0


lengthOn :: Foldable t => (a -> Bool) -> t a -> Int
lengthOn p = foldl' (\l x -> l + if p x then 1 else 0) 0

cartesian :: Applicative f => f a -> f b -> f (a, b)
cartesian xs ys = (,) <$> xs <*> ys

chunksOf :: Int -> [a] -> [[a]]
chunksOf k xs = case splitAt k xs of
                  (ys, []) -> [ys]
                  (ys, zs) -> ys : chunksOf k zs

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing


type Parser = Parsec String ()

numP :: Parser Int
numP = read <$> many1 digit

type Grid = Array (Int, Int)

gridP :: Parser a -> Parser (Grid a)
gridP p = do
  g <- many (many p <* newline)
  let (h, w) = (length g, length (g ! 0))
  return (Array.listArray ((0, 0), (h - 1, w - 1)) (concat g))

spaces :: Parser ()
spaces = many (char ' ') *> pure ()

newline :: Parser ()
newline = spaces *> char '\n' *> pure ()


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

    go i _ _ z | i == n = z
    go i l r z          = go (i + 1) l' r' (z |> zi)
      where
        c = if i < r then min (z ! (i - l)) (r - i) else 0
        !zi = match c
        match j = if ok then 1 + match (j + 1) else 0
          where ok = i + j < n && s' ! j == s' ! (i + j)
        (l', r') = if i + zi > r then (i, i + zi) else (l, r)


class Container c where
  type CIndex c
  type CItem c
  -- empty :: c
  singleton :: CItem c -> c
  (!) :: c -> CIndex c -> CItem c
  (<|) :: CItem c -> c -> c
  (|>) :: c -> CItem c -> c
  updateAt :: CIndex c -> CItem c -> c -> c
  -- (!?) :: c -> Index c -> Maybe (Item c)
  -- adjust :: Index c -> (Item c -> Item c) -> c -> c
  insertAt :: CIndex c -> CItem c -> c -> c
  -- delete :: Index c -> c -> c

instance IsList.IsList (Array Int e) where
  type Item (Array _ e) = e
  toList = Array.elems
  fromList xs = Array.listArray (0, length xs - 1) xs

instance Container (Array Int e) where
  type CIndex _ = Int
  type CItem (Array _ e) = e
  -- empty = error "not possible"
  singleton x = Array.listArray (0, 0) [x]
  (!) = (Array.!)
  x <| a = let (l, r) = Array.bounds a
            in Array.listArray (l - 1, r) (x : Array.elems a)
  a |> x = let (l, r) = Array.bounds a
            in Array.listArray (l, r + 1) (Array.elems a ++ [x])
  updateAt k x a = a Array.// [(k, x)]
  insertAt k x a = let (l, r) = Array.bounds a
                       (xs, ys) = splitAt k (Array.elems a)
                    in Array.listArray (l, r + 1) (xs ++ [x] ++ ys)

instance {-# OVERLAPPABLE #-} Array.Ix i => Container (Array i e) where
  type CIndex (Array i _) = i
  type CItem (Array _ e) = e

  singleton = undefined
  (<|) = undefined
  (|>) = undefined
  insertAt = undefined

  (!) = (Array.!)
  updateAt k x a = a Array.// [(k, x)]

instance Container [a] where
  type CIndex _ = Int
  type CItem [a] = a
  -- empty = []
  singleton x = [x]
  (!) = (!!)
  (<|) = (:)
  xs |> x = xs ++ [x]
  updateAt 0 y (_ : xs) = y : xs
  updateAt k y (x : xs) = x : updateAt (k - 1) y xs
  insertAt 0 y xs = y : xs
  insertAt _ _ [] = error "empty list"
  insertAt k y (x : xs) = x : insertAt (k - 1) y xs

instance Container (Seq a) where
  type CIndex _ = Int
  type CItem (Seq a) = a
  -- empty = []
  singleton x = [x]
  (!) = Seq.index
  (<|) = (Seq.<|)
  (|>) = (Seq.|>)
  updateAt = Seq.update
  insertAt = Seq.insertAt


class UnorderedContainer c where
  type UCItem c
  isElem :: UCItem c -> c -> Bool
  size :: c -> Int
  insert :: UCItem c -> c -> c
  delete :: UCItem c -> c -> c

instance Eq a => UnorderedContainer [a] where
  type UCItem [a] = a
  isElem = elem
  size = Prelude.length
  insert = (:)
  delete = List.delete

instance Eq e => UnorderedContainer (Array Int e) where
  type UCItem (Array _ e) = e
  isElem = elem
  size = length
  insert = (<|)
  delete x a = let (m, n) = Array.bounds a
                   (xs, ys) = span (/= x) (Array.elems a)
                in case ys of
                     []       -> a
                     (_ : zs) -> Array.listArray (m, n - 1) (xs ++ zs)

instance (Ord a, Hashable a) => UnorderedContainer (HashSet a) where
  type UCItem (HashSet a) = a
  isElem = HashSet.member
  size = HashSet.size
  insert = HashSet.insert
  delete = HashSet.delete

instance Eq a => UnorderedContainer (Seq a) where
  type UCItem (Seq a) = a
  isElem = elem
  size = Seq.length
  insert = (Seq.<|)
  delete x s = case Seq.findIndexL (== x) s of
                 Nothing -> s
                 Just i  ->  Seq.deleteAt i s


data Range a = Range a a
  deriving (Eq, Ord, Show)

-- TODO remove parser if different syntaxes for ranges occur
rangeP :: Parser a -> Parser (Range a)
rangeP p = Range <$> p <*> (char '-' *> p)

instance (Integral a, Ord a) => UnorderedContainer (Range a) where
  type UCItem (Range a) = a
  isElem x (Range a b) = a <= x && x <= b
  size (Range a b) = fromInteger (toInteger (b - a + 1))
  insert x r@(Range a b)
    | x < a        = Range x b
    | x > b        = Range a x
    | otherwise    = r
  delete x r@(Range a b)
    | not (x `isElem` r) = r
    | otherwise          = Range a x


infixr 3 .&&
infixr 2 .||

class BoolAlgebra a where
  (.&&), (.||) :: a -> a -> a
  neg :: a -> a

instance BoolAlgebra Bool where
  a .&& b = a && b
  a .|| b = a || b
  neg a = not a

instance BoolAlgebra b => BoolAlgebra (a -> b) where
  p .&& q = \x -> p x .&& q x
  p .|| q = \x -> p x .|| q x
  neg p = \x -> neg (p x)


deleteVertices :: Array Int Bool -> Graph -> Graph
deleteVertices vs g =
  let bs@(m, n) = Array.bounds g
      rem = filter (\v -> not (vs ! v))
      vs' = rem (Array.indices g)
      ixs = Array.array bs (zip vs' [m..])
      g' = Array.array (m, n - lengthOn id vs)
             [(ixs ! v, fmap (ixs !) (rem ws)) | (v, ws) <- Array.assocs g
                                               , not (vs ! v)
                                               ]
   in g'

