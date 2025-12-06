module Day06.A where

import MyPrelude


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
  rs <- transpose <$> many (many (spaces *> numP) <* newline)
  ss <- some (opP <* spaces)
  return (Homework (zipWith Problem rs ss))


solve :: Homework -> Int
solve (Homework ps) = sum (map solveP ps)
  where
    solveP (Problem xs Plus)  = sum xs
    solveP (Problem xs Times) = product xs


main :: IO ()
main = app homeworkP (print . solve)  -- 5227286044585

