module Day10.B where

import MyPrelude
import qualified Data.Map as Map
import Data.Scientific (toBoundedInteger)
import qualified Numeric.Optimization.MIP as MIP
import Numeric.Optimization.MIP ((.==.))
import Numeric.Optimization.MIP.Solver


data Machine = Machine String [[Int]] [Int]
  deriving Show

manP :: Parser Machine
manP = do
  let diagP = between (char '[') (char ']') (many (oneOf ".#"))
      semP  = between (char '(') (char ')') (numP `sepBy` char ',')
      reqP  = between (char '{') (char '}') (numP `sepBy` char ',')
  diag <- diagP <* spaces
  sems <- many (semP <* spaces)
  req <- reqP <* newline
  return (Machine diag sems req)


minPresses :: Machine -> IO Int
minPresses (Machine _ sems req) =
  let n = length req
      m = length sems
      vs = ['x' : show i | i <- [0..m - 1]]
      xs = map (MIP.varExpr . fromString) vs

      obj = sum xs

      cs = [0..n - 1] <&> \k ->
             let is = [i | (i, sem) <- zip [0..] sems, k `elem` sem]
              in sum (map (xs !!) is) .==. fromIntegral (req !! k)

      prob = MIP.def { MIP.objectiveFunction =
                         MIP.def { MIP.objDir = MIP.OptMin
                                 , MIP.objExpr = obj
                                 }
                     , MIP.constraints = cs
                     , MIP.varDomains =
                         Map.fromList (zip (map fromString vs)
                                           (repeat (MIP.IntegerVariable, (0, MIP.PosInf))))
                     }
   in do
     sol <- solve cbc (MIP.def { solveTimeLimit = Nothing }) prob
     return (fromJust (MIP.solObjectiveValue sol >>= toBoundedInteger))


main :: IO ()
main = app (many manP) ((print . sum =<<) . mapM minPresses)  -- 18681

