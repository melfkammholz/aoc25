module Day03.B where

import MyPrelude


newtype Bank = Bank (Array Int Int)

bankP :: Parser Bank
bankP = do
  bs <- some (read . singleton <$> digit)
  return (Bank (fromList bs))

banksP :: Parser [Bank]
banksP = some (bankP <* newline)


joltage :: Bank -> Int
joltage (Bank a) = table ! (n - 1, 12)
  where
    n = length a
    table = listArray ((0, 1), (n - 1, n)) [go i j | i <- [0..n - 1], j <- [1..n]]

    go 0 1              = a ! 0
    go i 1              = max (table ! (i - 1, 1)) (a ! i)
    go i k | i == k - 1 = table ! (i - 1, k - 1) * 10 + a ! i
    go i k              = max (table ! (i - 1, k)) (table ! (i - 1, k - 1) * 10 + a ! i)


main :: IO ()
main = app banksP (print . sum . map joltage)  -- 169019504359949

