module Main (main) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad
import Data.Maybe (fromJust)
import qualified Data.Map as M

data BoolExpr =   ITE BoolExpr BoolExpr BoolExpr
                | TERM Char
                | Zero
                | One
  deriving (Eq,Ord,Show)

-- data FoldOps a b = FoldOps { fold_or   :: b -> b -> b
--                            , fold_and  :: b -> b -> b
--                            , fold_not  :: b -> b
--                            , fold_term :: a -> b
--                            }
-- 
-- fold :: FoldOps a b -> BoolExpr -> b
-- fold f (OR p q)  = fold_or f (fold f p) (fold f q)
-- fold f (AND p q) = fold_and f (fold f p) (fold f q)
-- fold f (NOT p)   = fold_not f (fold f p)
-- fold f (TERM a)  = fold_term f a

-- distribute :: (BoolExpr -> BoolExpr -> BoolExpr) -> BoolExpr -> BoolExpr -> BoolExpr
-- distribute op p (OR q0 q1)  = OR (distribute op p q0) (distribute op p q1)
-- distribute op p (AND q0 q1) = AND (distribute op p q0) (distribute op p q1)
-- distribute op (OR p0 p1) q  = OR (distribute op p0 q) (distribute op p1 q)
-- distribute op (AND p0 p1) q = AND (distribute op p0 q) (distribute op p1 q)
-- distribute op p q           = p `op` q

-- newtype CNF a = CNF BoolExpr
-- -- | http://en.wikipedia.org/wiki/Disjunctive_normal_form
-- cnf :: BoolExpr -> CNF a
-- cnf = CNF . fold (FoldOps f g h w)
--   where f = mkor
--         g = distribute AND
--         h = mknot
--         w = TERM

-- newtype DNF a = DNF BoolExpr
-- -- | http://en.wikipedia.org/wiki/Conjunctive_normal_form
-- dnf :: BoolExpr -> DNF a
-- dnf = DNF . fold (FoldOps f g h w)
--   where f = distribute OR 
--         g = mkand
--         h = mknot
--         w = TERM

mkiff :: BoolExpr -> BoolExpr -> BoolExpr
mkiff p q = (mkimplies p q) `mkand` (mkimplies q p)

mkimplies :: BoolExpr -> BoolExpr -> BoolExpr
mkimplies p q = mkite p q One

mknot :: BoolExpr -> BoolExpr
mknot p = mkite p Zero One

mkor :: BoolExpr -> BoolExpr -> BoolExpr
mkor p q = mkite p One q

mkand :: BoolExpr -> BoolExpr -> BoolExpr
mkand p q = mkite p q Zero

mkite :: BoolExpr -> BoolExpr -> BoolExpr -> BoolExpr
mkite Zero _ h = h
mkite One g _  = g
mkite f One h  | h<f       = ITE h One f
               | otherwise = ITE f One h
mkite f g Zero | g<f       = ITE g f Zero
               | otherwise = ITE f g Zero
mkite f g One  | g<f       = ITE (mknot g) (mknot f) One
               | otherwise = ITE f g One
mkite f Zero h | h<f       = ITE (mknot h) Zero (mknot f)
               | otherwise = ITE f Zero h
mkite f g h    = ITE f g h

parser :: State (String,M.Map Char Bool) BoolExpr
parser = get >>= \(s,v) ->
         case s
         of ('N':xs) -> let (p,s0) = runState parser (xs,v)
                        in do put s0
                              return (mknot p)
            ('D':xs) -> let (p,s0) = runState parser (xs,v)
                            (q,s1) = runState parser s0
                        in do put s1
                              return (mkor p q)
            ('C':xs) -> let (p,s0) = runState parser (xs,v)
                            (q,s1) = runState parser s0
                        in do put s1
                              return (mkand p q)
            ('I':xs) -> let (p,s0) = runState parser (xs,v)
                            (q,s1) = runState parser s0
                        in do put s1
                              return (mkimplies p q)
            ('E':xs) -> let (p,s0) = runState parser (xs,v)
                            (q,s1) = runState parser s0
                        in do put s1
                              return (mkiff p q)
            (x:xs)   -> do put (xs,M.insert x False v)
                           return (TERM x)
            _        -> error "parser: parse error"

eval :: BoolExpr -> Reader (M.Map Char Bool) Bool
eval (ITE i t e) = eval i >>= \cond ->
                   if cond
                   then eval t
                   else eval e
eval (TERM k)    = fmap (fromJust . M.lookup k) ask
eval Zero        = return False
eval One         = return True

compute :: Reader (M.Map Char Bool) Bool -> M.Map Char Bool -> Bool
compute circuit = runReader circuit

tautology :: String -> Bool
tautology s = checktaut keys vars
  where (expr,(_,vars))     = runState parser (s,M.empty)
        keys                = M.keys vars
        circuit             = compute (eval expr)
        checktaut [] _      = True
        checktaut (k:ks) vf =    circuit vt
                              && circuit vf
                              && checktaut ks vf
                              && checktaut ks vt
          where vt = M.insert k True vf

main :: IO ()
main = do n <- fmap read getLine
          interact (unlines . map ((\v -> if v then "YES" else "NO") . tautology) . take n . lines)

