import qualified Char as C

txt2c :: String -> Maybe String
txt2c []     = Just []
txt2c (t:ts) | t=='_'      = Nothing
             | C.isUpper t = txt2c ts >>= \v -> Just $ '_' : C.toLower t : v
             | otherwise   = txt2c ts >>= \v -> Just $ t : v

txt2j :: Bool -> String -> Maybe String
txt2j True  [] = Nothing
txt2j False [] = Just []
txt2j u (t:ts) | u && C.isLower t = txt2j False ts >>= \v -> Just $ C.toUpper t : v
               | t=='_'           = if u then Nothing else txt2j True ts
               | C.isUpper t      = Nothing
               | otherwise        = txt2j False ts >>= \v -> Just $ t : v

transform :: String -> Maybe String
transform s = let (l,r) = break (\c -> c=='_' || C.isUpper c) s
              in if (null l) then Nothing else case (take 1 r)
                                               of []    -> Just l
                                                  ['_'] -> txt2j True (drop 1 r) >>= \r1 -> Just $ l ++ r1
                                                  _     -> txt2c r >>= \r1 -> Just $ l ++ r1

main = interact (unlines . map (\v -> case v of {Nothing -> "Error!"; (Just v) -> v;}). map transform . lines)
