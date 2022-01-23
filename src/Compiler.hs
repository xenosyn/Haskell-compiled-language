{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine ( Instr(LOADI,LOAD,ADD,STORE,JMP,JMPLESS,JMPGE) )
import Interpreter ( Com(Assign,Seq,If,While,SKIP), BExp(Bc,Not,And,Less), AExp(N,V,Plus) )

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N num) = [LOADI num]
acomp (V var) = [LOAD var]
acomp (Plus a1 a2) = acomp a1 ++ acomp a2 ++ [ADD]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc x) y n = [JMP n | x == y]
bcomp (Less a b) y n = if y then acomp a ++ acomp b ++ [JMPLESS n] else acomp a ++ acomp b ++ [JMPGE n]
bcomp (Not (Bc x)) y n = [JMP n | not x == y]
-- make clear
bcomp (And (Bc x) (Bc y)) z n
  | y = if (x && y) == z then [JMP n] else [JMP 1, JMP n]
  | (x && y) == z = [JMP n]
  | otherwise = []

bcomp (And (Less a1 a2) (Bc x)) y n
  | y = if x then acomp a1 ++ acomp a2 ++ [JMPLESS n] else [JMP n] ++ acomp a1 ++ acomp a2 ++ [JMPLESS n]
  | x = acomp a1 ++ acomp a2 ++ [JMPGE n]
  | otherwise = [JMP (n+length (acomp a1 ++ acomp a2 ++ [JMPGE n]))] ++ acomp a1 ++ acomp a2 ++ [JMPGE n]


bcomp (And (Bc x) (Less a1 a2)) y n
  | y = if x then acomp a1 ++ acomp a2 ++ [JMPLESS n]  else [JMP n] ++ acomp a1 ++ acomp a2 ++ [JMPLESS n]
  | x = acomp a1 ++ acomp a2 ++ [JMPGE n]
  | otherwise = [JMP (n+length (acomp a1 ++ acomp a2 ++ [JMPGE n]))] ++ acomp a1 ++ acomp a2 ++ [JMPGE n]


--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign x a) = acomp a ++ [STORE x]
ccomp (Seq c1 c2) = ccomp c1 ++ ccomp c2
ccomp (If b c1 c2) = bcomp b False (length (ccomp c1)+1) ++ ccomp c1 ++ [JMP (length (ccomp c2))] ++ ccomp c2
ccomp (While b c) = bcomp b False (length (ccomp c)+1) ++ ccomp c ++ [JMP (-(length (bcomp b False (length (ccomp c)+1)) + length (ccomp c) + 1))]
ccomp SKIP = []



