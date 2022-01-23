{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map as Map
import Machine

--TODO Task 2.1
data AExp =
    N Int | V String | Plus AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (V var) state = removeMaybe (Map.lookup var state)
aval (N num) state = num
aval (Plus a1 a2) state = aval a1 state + aval a2 state

removeMaybe :: Maybe Int -> Int
removeMaybe (Just i) = i

--TODO Task 2.1
data BExp =
    Bc Bool | Not BExp | And BExp BExp | Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc bool) state = bool
bval (Not (Bc bool)) state = not bool
bval (And b1 b2) state =  bval b1 state && bval b2 state
bval (Less a1 a2) state = aval a1 state < aval a2 state

--TODO Task 2.1
data Com =
    Assign Vname AExp | Seq Com Com | If BExp Com Com | While BExp Com | SKIP
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
eval (Assign v x) state = Map.insert v (aval x state) state
eval (Seq c1 c2) state = do
    let s1 = eval c1 state
    eval c2 s1
eval (If b c1 c2) state = if bval b state then eval c1 state else eval c2 state
--eval (While b c) state | (bval b state) == False = state | (bval b state) == True = (eval While (bval b state) (eval c state) state)
eval (While b c) state = if bval b state then eval (While b c) (eval c state) else state
eval SKIP state = state
