
-- Original Grammar:
-- Set          ::= "{" Elementlist "}"
-- Elementlist  ::= <empty> | List
-- List         ::= Element | Element "," List
-- Element      ::= Atom | Set
-- Atom         ::= "{" | "}" | ","

import qualified Control.Monad as C
import qualified Control.Monad.State as S
import Debug.Trace

type Token  = Char
type MState = S.State ([PState],[Token]) Bool

data PState =   State Token
              | Final

parse :: [Token] -> Bool
parse input = let state = ([],input)
              in (S.evalState pst0) state

pst0 :: MState
pst0 = S.get >>= \state ->
       case state
       of (stack,('{':input)) -> S.put (State '{':stack,input) >> pst1
          _                   -> S.put ([],[]) >> return False

pst1 :: MState
pst1 = S.get >>= \state ->
       case state
       of (stack,(c:input)) -> S.put (State c:stack,input) >> pst2
          _                 -> return False

pst2 :: MState
pst2 = S.get >>= \state ->
       case state
       of ([State '}',State '{'],[]) -> S.put ([],[]) >> return True
          (stack,(',':c:input))      -> S.put (State c:tail stack,input) >> pst3
          (stack,(c:input))          -> S.put (State c:stack,input) >> pst3
          _                          -> return False

pst3 :: MState
pst3 = undefined
