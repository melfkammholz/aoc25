{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module MyPrelude (
         app,
         hornerL, hornerR,
         cartesian, chunksOf, lengthOn, find, foldl', sort, sortOn, transpose,
         unsnoc, (\\), nub,
         pattern Sorted,

         module Control.Applicative,

         Parser, numP, spaces, Grid, gridP,

         sepBy, oneOf, char, digit, newline, between, string, letter,

         module Utils.ModInt,

         module Utils.ZAlg,

         Container(..),
         UnorderedContainer(..),
         Array, Array.bounds, Array.listArray,
         HashSet,
         fromList, toList,
         Range(..), rangeP,
         Seq,

         BoolAlgebra(..),

         Graph, Graph.Vertex, Graph.graphFromEdges, Graph.indegree, Graph.topSort,
         Graph.vertices,
         deleteVertices,

         Vec3(..), vec3X, vec3Y, vec3Z, norm2sq, dist2sq,

         minmax,

         fromString,

         fromJust,
         (<&>)
       ) where

import Utils.ModInt
import Utils.ZAlg

import Control.Applicative
import Data.List (find, foldl', sort, sortOn, transpose, (\\), nub)
import Data.String
import Data.Maybe
import Data.Foldable (Foldable)
import Data.Functor ((<&>))
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
import qualified GHC.IsList as IsList
import GHC.IsList (fromList, toList)
import System.CPUTime
import Text.Parsec hiding (many, newline, spaces)
import Text.Parsec.Char (char)
import Text.Printf


app :: Parser a -> (a -> IO ()) -> IO ()
app p f = do
  !inp <- parse p "<stdin>" <$> getContents
  either print (time . f) inp

time :: IO a -> IO a
time act = do
  t1 <- getCPUTime
  !x <- act
  t2 <- getCPUTime
  printf "%s\n" (niceTime (fromInteger (t2 - t1) :: Double))
  return x

niceTime :: Double -> String
niceTime = go ["ps", "ns", "μs", "ms", "s"]
  where
    go (u:us) x | x > 1000  = go us (x / 1000)
                | otherwise = show x ++ u


pattern Sorted :: Ord a => [a] -> [a]
pattern Sorted xs <- (sort -> xs)

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
  g <- many (some p <* newline)
  let (h, w) = (length g, length (g ! 0))
  return (Array.listArray ((0, 0), (h - 1, w - 1)) (concat g))

spaces :: Parser ()
spaces = many (char ' ') *> pure ()

newline :: Parser ()
newline = spaces *> char '\n' *> pure ()


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


data Vec3 a = Vec3 !a !a !a
  deriving (Eq, Ord, Show)

vec3X :: Vec3 a -> a
vec3X (Vec3 x _ _) = x

vec3Y :: Vec3 a -> a
vec3Y (Vec3 _ y _) = y

vec3Z :: Vec3 a -> a
vec3Z (Vec3 _ _ z) = z

instance Num a => Num (Vec3 a) where
  Vec3 x1 y1 z1 + Vec3 x2 y2 z2 = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  Vec3 x1 y1 z1 - Vec3 x2 y2 z2 = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  Vec3 x1 y1 z1 * Vec3 x2 y2 z2 = Vec3 (y1 * z2 - z1 * y2)
                                       (z1 * x2 - x1 * z2)
                                       (x1 * y2 - y1 * x2)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger x = let y = fromInteger x in Vec3 y y y

norm2sq :: Num a => Vec3 a -> a
norm2sq (Vec3 x y z) = x * x + y * y + z * z

dist2sq :: Num a => Vec3 a -> Vec3 a -> a
dist2sq x y = norm2sq (x - y)


{-# INLINE minmax #-}
{-# SPECIALIZE minmax :: Int -> Int -> (Int, Int) #-}
minmax :: Ord a => a -> a -> (a, a)
minmax a b = if a < b then (a, b) else (b, a)


