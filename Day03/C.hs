module Day03.C where

import MyPrelude


newtype Bank = Bank [Int]

bankP :: Parser Bank
bankP = do
  bs <- some (read . singleton <$> digit)
  return (Bank bs)

banksP :: Parser [Bank]
banksP = some (bankP <* newline)


joltage :: Bank -> Int
joltage (Bank xs) = go xs [] 12 (length xs)
  where
    go []     s      k _             = hornerR (drop (length s - 12) s)
    go xs     ys     k n | k == n    = go [] (reverse xs ++ ys) 0 0
    go (x:xs) []     k n             = go xs [x] (k - 1) (n - 1)
    go (x:xs) (y:ys) k n | x <= y    = go xs (x : y : ys) (k - 1) (n - 1)
                         | otherwise = go (x : xs) ys (k + 1) n


main :: IO ()
main = app banksP (print . sum . map joltage)  -- 169019504359949

