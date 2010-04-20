import qualified Data.ByteString.Lazy.Char8 as B

infix2rpn = parse []
  where
    ops                  = ['+','-','*','/','^']
    gt [] _              = False
    gt (e:es) o | e=='+' = False
                | e=='-' = False
                | e=='(' = False
                | e=='*' = o/='^'
                | e=='/' = o/='^'
                | e=='^' = True
    parse sk s | B.null s   = B.pack sk
               | t=='('     = parse (t:sk) rs
               | t==')'     = let (l,rsk) = break (=='(') sk
                              in B.append (B.pack l) (parse (tail rsk) rs)
               | elem t ops = if (gt sk t) then
                                head sk `B.cons` parse (tail sk) s
                              else
                                parse (t:sk) rs
               | otherwise  = t `B.cons` parse (sk) rs
      where
        (t1,rs) = B.splitAt 1 s
        t       = B.head t1

main = getLine >>= \n -> 
  B.interact (B.unlines . map infix2rpn . take (read n) . B.lines)
