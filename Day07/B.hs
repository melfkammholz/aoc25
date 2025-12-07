module Day07.B where

import MyPrelude
import Data.Array (bounds)


newtype Diagram = Diagram (Array (Int, Int) Char)
  deriving Show

diagramP :: Parser Diagram
diagramP = do
  g <- many (many (oneOf "S.^") <* newline)
  let (h, w) = (length g, length (g ! 0))
      a      = listArray ((0, 0), (h - 1, w - 1)) (concat g)
  return (Diagram a)


timelines :: Diagram -> Int
timelines (Diagram g) = sum [dp ! (h, x) | x <- [0..w]]
  where
    bs@(_, (h, w)) = bounds g
    Just s = find (\x -> g ! (0, x) == 'S') [0..w]

    dp = listArray bs [go y x | y <- [0..h], x <- [0..w]]

    go 0 x = if x == s then 1 else 0
    go y x =
      case g ! (y, x) of
        '^' -> dp ! (y - 1, x)
        '.' -> sum [ if 0 <= x - 1 && g ! (y - 1, x - 1) == '^' then dp ! (y - 1, x - 1) else 0
                   , if x + 1 <= w && g ! (y - 1, x + 1) == '^' then dp ! (y - 1, x + 1) else 0
                   , if g ! (y - 1, x) /= '^' then dp ! (y - 1, x) else 0
                   ]


main :: IO ()
main = app diagramP (print . timelines)  -- 7759107121385

