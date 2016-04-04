module DFA where

import Prelude hiding (Word)

type State = Int
type Alphabet a = [a]
type DFA a = 
  ( Alphabet a             -- alphabet
  , State                  -- initial state
  , State -> a -> State    -- transition function
  , State -> Bool)         -- test for final state
type Word a = [a]

alphabet :: DFA a -> Alphabet a
alphabet (x, _, _, _) = x

initial :: DFA a -> State
initial (_, x, _, _) = x

transition :: DFA a -> (State -> a -> State)
transition (_, _, x, _) = x

finalState :: DFA a -> State -> Bool
finalState (_, _, _, x) = x

{-
   Accessor functions are useful because the abstract away the
   underlaying primitive data type with a functional interface
   that makes the code more robust against potential future
   expansions and simultaneously more readable since we can now
   use descriptive names to retrieve the elements of the data
   structure.
-}

accepts :: DFA a -> Word a -> Bool
accepts dfa = finalState dfa . foldl (transition dfa) (initial dfa)

lexicon :: Alphabet a -> Int -> [Word a]
lexicon _ 0 = [[]]
lexicon a c = concatMap (\x -> map ((:) x) (lexicon a (c - 1))) a

language :: DFA a -> Int -> [Word a]
language dfa = filter (accepts dfa) . lexicon (alphabet dfa)

-- Try to use map, foldl, foldr, filter and/or list comprehensions.
