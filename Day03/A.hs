module Day03.A where

import MyPrelude


newtype Bank = Bank (Array Int Int)

bankP :: Parser Bank
bankP = do
  bs <- some (read . singleton <$> digit)
  return (Bank (fromList bs))

banksP :: Parser [Bank]
banksP = some (bankP <* newline)


joltage :: Bank -> Int
joltage (Bank a) = go 0 1 2
  where
    n = length a
    go i j k | k >= n                      = (a ! i) * 10 + (a ! j)
             | k + 1 < n && a ! i < a ! k  = go k (k + 1) (k + 1)
             | k < n && a ! j < a ! k      = go i k k
             | otherwise                   = go i j (k + 1)


main :: IO ()
main = app banksP (print . sum . map joltage)  -- 17087

