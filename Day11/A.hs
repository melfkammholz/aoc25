module Day11.A where

import MyPrelude
import Control.Monad
import Control.Monad.ST (runST)
import qualified Data.HashSet as HashSet
import qualified Data.Vector.Unboxed.Mutable as MVU


graphP :: Parser (Graph, Vertex -> ((), String, [String]), String -> Maybe Vertex)
graphP = do
  let adjP = (,) <$> (many letter <* string ": ")
                 <*> (many letter `sepBy` char ' ' <* newline)
  adjs <- many adjP
  let ms = toList (fromList (concatMap snd adjs) `HashSet.difference` fromList (map fst adjs))
      adjs' = adjs ++ [(m, []) | m <- ms]
  return (graphFromEdges [((), v, ws) | (v, ws) <- adjs'])


paths :: (Graph, Vertex -> ((), String, [String]), String -> Vertex) -> Int
paths (graph, nodeFromVertex, vertexFromKey) = runST $ do
    let n = length graph
        [you, out] = vertexFromKey <$> ["you", "out"]
    dp <- MVU.new n
    MVU.unsafeWrite dp you 1
    forM_ (topSort graph) $ \v -> do
      let (_, _, ws) = nodeFromVertex v
      m <- MVU.unsafeRead dp v
      forM_ ws (MVU.unsafeModify dp (+ m) . vertexFromKey)
    MVU.unsafeRead dp out


main :: IO ()
main = app graphP (print . paths . fmap (fromJust .))  -- 574

