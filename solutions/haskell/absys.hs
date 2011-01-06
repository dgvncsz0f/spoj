-- https://www.spoj.pl/problems/ABSYS/

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Control.Monad as M

solve :: Maybe Int -> Maybe Int -> Maybe Int -> [Int]
solve Nothing (Just b) (Just r)  = [r-b, b, r]
solve (Just a) Nothing (Just r)  = [a, r-a, r]
solve (Just a) (Just b) Nothing  = [a, b, a+b]
solve (Just a) (Just b) (Just r) = [a, b, r]

readterm :: L.ByteString -> Maybe Int
readterm s = case L.find (=='m') s
            of (Just _) -> Nothing
               Nothing  -> case (L.readInt s)
                           of (Just (v,_)) -> Just v
                              Nothing      -> Nothing -- should not happen

main = do
  input <- L.getContents
  let linez = map (L.words) (L.lines input)
  M.forM (filter ((==5) . length) linez) $ \tokens -> do
      let (as,bs,rs) = (tokens !! 0, tokens !! 2, tokens !! 4)
          solved     = solve (readterm as) (readterm bs) (readterm rs)
      putStrLn $ foldr (++) [] (zipWith (++) ["", " + ", " = "] (map show solved))
  return ()
