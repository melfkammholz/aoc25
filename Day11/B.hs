module Day11.B where

import MyPrelude
import Control.Monad
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed.Mutable as MVU


graphP :: Parser (Graph, Vertex -> ((), String, [String]), String -> Maybe Vertex)
graphP = do
  let adjP = (,) <$> (many letter <* string ": ")
                 <*> (many letter `sepBy` char ' ' <* newline)
  adjs <- many adjP
  let ms    = nub (filter (`notElem` map fst adjs) (concatMap snd adjs))
      adjs' = adjs ++ [(m, []) | m <- ms]
  return (graphFromEdges [((), v, ws) | (v, ws) <- adjs'])


paths :: (Graph, Vertex -> ((), String, [String]), String -> Vertex) -> Int
paths (graph, nodeFromVertex, vertexFromKey) = runST $ do
    let n = length graph
        [svr, out, dac, fft] = vertexFromKey <$> ["svr", "out", "dac", "fft"]
    dp <- MVU.new n
    MVU.unsafeWrite dp svr 1
    forM_ (topSort graph) $ \v -> do
      when (v == fft || v == dac) $ do
        forM_ (vertices graph) $ \w ->
          when (v /= w) (MVU.unsafeWrite dp w 0)
      let (_, _, ws) = nodeFromVertex v
      m <- MVU.unsafeRead dp v
      mapM_ (MVU.unsafeModify dp (+ m) . vertexFromKey) ws
    MVU.unsafeRead dp out


main :: IO ()
main = app graphP (print . paths . fmap (fromJust .))  -- 306594217920240

