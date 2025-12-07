module Day07.A where

import MyPrelude


newtype Diagram = Diagram (Grid Char)
  deriving Show

diagramP :: Parser Diagram
diagramP = Diagram <$> gridP (oneOf "S.^")


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

