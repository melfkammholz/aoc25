module Day06.B where

import MyPrelude
import Text.Parsec (State(..), updateParserState)


data Op = Plus | Times
  deriving Show

data Problem = Problem [Int] Op
  deriving Show

newtype Homework = Homework [Problem]
  deriving Show

opP :: Parser Op
opP = char '+' *> pure Plus <|> char '*' *> pure Times

homeworkP :: Parser Homework
homeworkP = do
  rs <- many (numP <* newline) `sepBy` newline
  ss <- some opP
  return (Homework (zipWith Problem rs ss))


solve :: Homework -> Int
solve (Homework ps) = sum (map solveP ps)
  where
    solveP (Problem xs Plus) = sum xs
    solveP (Problem xs Times) = product xs


inputT :: State String u -> State String u
inputT s = s { stateInput = filter (/= ' ') (unlines (transpose nums) ++ ops) }
  where Just (nums, ops) = unsnoc (lines (stateInput s))

main :: IO ()
main = app (updateParserState inputT >> homeworkP) (print . solve)  -- 10227753257799

