import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Monad as M

data OpT = Add
         | Sub
         | Mul
         | Div
  deriving (Eq)

data AST =   Branch AST OpT AST
           | Symbol String
  deriving (Eq)

data TokenT =   TkAdd
              | TkSub
              | TkMul
              | TkDiv
              | TkLp
              | TkRp
              | TkId String
              | TkEOF
  deriving (Show,Eq)

instance Ord OpT where
  compare Add Add = EQ
  compare Add Sub = EQ
  compare Add Mul = LT
  compare Add Div = LT
  compare Sub Add = GT
  compare Sub Sub = GT
  compare Sub Mul = LT
  compare Sub Div = LT
  compare Mul Add = GT
  compare Mul Sub = GT
  compare Mul Mul = EQ
  compare Mul Div = EQ
  compare Div _   = GT

castop :: TokenT -> Maybe OpT
castop TkAdd = Just Add
castop TkSub = Just Sub
castop TkDiv = Just Div
castop TkMul = Just Mul
castop _     = Nothing

instance Show OpT where
  showsPrec _ = show_op'
    where
      show_op' Add = ('+':)
      show_op' Sub = ('-':)
      show_op' Div = ('/':)
      show_op' Mul = ('*':)

instance Show AST where
  showsPrec _ = show_ast'
    where 
      shows'  (Symbol a)      = (a++)
      shows'  (Branch l o r)  = shows l . shows o . shows r
      showsl' (Branch l o r)  = ('(':) . shows l . (')':) . shows o . shows r
      showsr' (Branch l o r)  = shows l . shows o . ('(':) . shows r . (')':)
      showslr' (Branch l o r) = ('(':) . shows l . (')':) . shows o . ('(':) . shows r . (')':)

      show_ast' s@(Symbol _)                            = shows' s
      show_ast' s@(Branch (Symbol _) o (Symbol _))      = shows' s
      show_ast' s@(Branch (Symbol _) o (Branch _ o1 _)) = if (o>o1) then
                                                            showsr' s
                                                          else
                                                            shows' s
      show_ast' s@(Branch (Branch _ o0 _) o (Symbol _)) = if (o0<o) then
                                                            showsl' s
                                                          else
                                                            shows' s
      show_ast' s@(Branch (Branch _ o0 _) o (Branch _ o1 _)) = if (o0<o) then
                                                                 if (o>o1) then
                                                                   showslr' s
                                                                 else
                                                                   showsl' s
                                                               else
                                                                 if (o>o1) then
                                                                   showsr' s
                                                                 else
                                                                   shows' s

instance Ord TokenT where
  compare TkLp _  = LT
  compare _ TkLp  = LT
  compare TkEOF _ = LT
  compare _ TkEOF = GT
  compare a b     = let a1 = castop a
                        b1 = castop b
                    in case (a1,b1)
                       of (Just a2,Just b2) -> compare a2 b2
                          _                 -> error "undefined"

rpn2ast :: [TokenT] -> AST
rpn2ast = head . parse' []
  where
    parse' sk []            = sk
    parse' sk ((TkId v):ts) = parse' (Symbol v : sk) ts
    parse' sk (o:ts)        = let ([r,l],rsk) = splitAt 2 sk
                              in case (castop o) 
                                 of (Just o1) -> parse' (Branch l o1 r : rsk) ts
                                    Nothing   -> error "undefined"

infix2rpn :: B.ByteString -> [TokenT]
infix2rpn = parse' [TkEOF]
  where
    oplist' = [('(',TkLp),(')',TkRp),('+',TkAdd),('-',TkSub),('*',TkMul),('/',TkDiv)]

    readsym' s | B.null s                 = ([],B.empty)
               | elem t (map fst oplist') = ([],s)
               | otherwise                = let (sym,rts) = readsym' ts
                                            in  (t:sym,rts)
      where
        t  = B.head s
        ts = B.tail s

    lexical' s | B.null s  = (TkEOF,B.empty)
               | otherwise = case lookup t oplist'
                             of (Just tk) -> (tk, ts)
                                Nothing   -> let (sym,rts) = readsym' s
                                             in (TkId sym, rts)
      where
        t  = B.head s
        ts = B.tail s
    
    parse' sk@(a:as) ip | B.null ip = init sk
                        | otherwise = let (b,rip)   = lexical' ip
                                          (skl,skr) = break (==TkLp) sk
                                      in case b
                                         of b1@(TkId _) -> b1 : parse' sk rip
                                            TkLp        -> parse' (TkLp:sk) rip
                                            TkRp        -> skl ++ parse' (tail skr) rip
                                            _           -> if (a>b) then
                                                             a : parse' as ip
                                                           else
                                                             parse' (b:sk) rip

bsread s = case (B.readInt s)
           of (Just (v,_)) -> v
              Nothing      -> error "parse error"

main = do
  input <- B.getContents
  let linez    = B.lines input
      tcases   = bsread (head linez)
      exprs    = take tcases (tail linez)
      redux    = rpn2ast . infix2rpn
  M.forM exprs (putStrLn . show . redux)
  return ()
