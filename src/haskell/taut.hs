module Main (main) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad
import Data.Maybe (fromJust)
import qualified Data.Map as M

data BoolExpr =   OR BoolExpr BoolExpr
                | AND BoolExpr BoolExpr
                | NOT BoolExpr
                | TERM Char
                | Zero
                | One
  deriving (Eq)

mkiff :: BoolExpr -> BoolExpr -> BoolExpr
mkiff p q = (p `mkimplies` q) `mkand` (q `mkimplies` p)

mkimplies :: BoolExpr -> BoolExpr -> BoolExpr
mkimplies p q = mknot p `mkor` q

mknot :: BoolExpr -> BoolExpr
mknot (NOT p) = p
mknot Zero    = One
mknot One     = Zero
mknot p       = NOT p

mkor :: BoolExpr -> BoolExpr -> BoolExpr
mkor One _  = One
mkor _ One  = One
mkor Zero q = q
mkor p Zero = p
mkor p q | p == (mknot q) = One
         | p == q         = Zero
         | otherwise      = p `OR` q

mkand :: BoolExpr -> BoolExpr -> BoolExpr
mkand Zero _ = Zero
mkand _ Zero = Zero
mkand One q  = q
mkand p One  = p
mkand p q | p == (mknot q) = Zero
          | p == q         = Zero
          | otherwise      = p `AND` q

eval :: BoolExpr -> Reader (M.Map Char Bool) Bool
eval (AND p q) = do pv <- eval p
                    qv <- eval q
                    return (pv && qv)
eval (OR p q)  = do pv <- eval p
                    qv <- eval q
                    return (pv || qv)
eval (NOT p)   = do pv <- eval p
                    return (not pv)
eval (TERM k)  = fmap (fromJust . M.lookup k) ask
eval Zero      = return False
eval One       = return True

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

main :: IO ()
main = do n <- fmap read getLine
          interact (unlines . map ((\v -> if v then "YES" else "NO") . tautology) . take n . lines)
