module Day07.A where

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


splits :: Diagram -> Int
splits (Diagram g) = lengthOn id [spl y x | y <- [0..h], x <- [0..w]]
  where
    bs@(_, (h, w)) = bounds g
    Just s = find (\x -> g ! (0, x) == 'S') [0..w]

    dp = listArray bs [go y x | y <- [0..h], x <- [0..w]]

    spl y x = g ! (y, x) == '^' && dp ! (y, x)

    go 0 x = x == s
    go y x =
      case g ! (y, x) of
        '^' -> dp ! (y - 1, x)
        '.' -> or [ 0 <= x - 1 && spl (y - 1) (x - 1)
                  , x + 1 <= w && spl (y - 1) (x + 1)
                  , dp ! (y - 1, x) && g ! (y - 1, x) /= '^'
                  ]


main :: IO ()
main = app diagramP (print . splits)  -- 1638

