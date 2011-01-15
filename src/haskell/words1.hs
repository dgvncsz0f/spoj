module Main where

import qualified Data.Map as M

type Index = M.Map Char [(String,Int)]


build = map (\w -> (key w,value w)
  where key     = last
        value w = (w,0)
-- mkindex :: [String] -> Index
-- mkindex ws = build keys (M.fromList . flip zip (repeat 0) $ keys)
--   where keys = map newkey ws
-- 
--         build (k:ks) map = let vertice = 

