import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@=?) )
import Machine
    ( exec,
      iexec,
      Instr(JMP, JMPLESS, JMPGE, LOAD, LOADI, ADD, STORE) )
import Interpreter
    ( aval,
      bval,
      eval,
      AExp(N, Plus, V),
      BExp(Less, Not, And, Bc),
      Com(Assign, Seq, SKIP, If, While) )
import Compiler ( acomp, bcomp, ccomp )
import Data.Map ( empty, fromList )
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [machine,interpreter,compiler]

machine = testGroup "Machine Tests"
  [ testCase "Single Instruction" $
    (1,fromList [],[5]) @=? iexec (LOADI 5) (0, empty, [])
  , testCase "Single Instruction" $
    (1,fromList [("v1",5)],[5]) @=? iexec (LOAD "v1") (0, fromList [("v1",5)], [])
  , testCase "Single Instruction" $
    (1,fromList [],[11]) @=? iexec ADD (0, empty, [5,6])
  , testCase "Single Instruction" $
    (1,fromList [("x",5)],[]) @=? iexec (STORE "x") (0, empty, [5])
  , testCase "Single Instruction" $
    (6,fromList [],[]) @=? iexec (JMP 5) (0, empty, [])
  , testCase "Single Instruction" $
    (1,fromList [],[]) @=? iexec (JMPLESS 5) (0, empty, [5,6])
  , testCase "Single Instruction" $
    (6,fromList [],[]) @=? iexec (JMPGE 5) (0, empty, [5,6])
  , testCase "Multiple Instructions" $
    (3,fromList [],[3]) @=? exec [LOADI 1, LOADI 2, ADD] (0,empty,[])
  , testCase "Multiple Instructions" $
    (4,fromList [("v1",1),("v2",2)],[]) @=? exec [LOADI 1, STORE "v1", LOADI 2, STORE "v2"] (0,empty,[])
  ]

interpreter = testGroup "Interpreter Tests"
  [ testCase "Arithmetic Expressions" $
    3 @=? aval (Plus (N 3) (V "x")) (fromList [("x",0)])
  , testCase "Boolean Expressions" $
    False @=? bval (Less (N 3) (V "x")) (fromList [("x",0)])
  , testCase "Commands" $
    fromList [] @=? eval SKIP (fromList [])
  , testCase "Commands" $
    fromList [("x",5)] @=? eval (Assign "x" (N 5)) (fromList [("x",0)])
  , testCase "Commands" $
    fromList [("x",6)] @=? eval (Seq (Assign "x" (N 5)) (Assign "x" (N 6))) (fromList [("x",0)])
  , testCase "Commands" $
    fromList [("x",6)] @=? eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x",4)])
  , testCase "Commands" $
    fromList [("x",10)] @=? eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x",10)])    
  , testCase "Commands" $
    fromList [("x",5)] @=? eval (While (Less (V "x") (N 5)) (Assign "x" (Plus (V "x") (N 1)))) (fromList [("x",0)])
  ]

compiler = testGroup "Compiler Tests"
  [ testCase "Arithmetic Expressions" $
    [LOADI 5,LOAD "x",ADD] @=? acomp (Plus (N 5) (V "x"))
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (Bc True) True 3
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (Bc False) False 3
  , testCase "Boolean Expressions" $
    [] @=? bcomp (Bc True) False 3
  , testCase "Boolean Expressions" $
    [] @=? bcomp (Not (Bc False)) False 3
  , testCase "Boolean Expressions" $
    [] @=? bcomp (And (Bc True) (Bc False)) True 3
  , testCase "Boolean Expressions" $
    [JMP 1,JMP 3] @=? bcomp (And (Bc False) (Bc True)) True 3
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (And (Bc True) (Bc False)) False 3
  , testCase "Boolean Expressions" $
    [JMP 3] @=? bcomp (And (Bc False) (Bc True)) False 3
  , testCase "Boolean Expressions" $
    [LOAD "x",LOADI 5,JMPLESS 3] @=? bcomp (Less (V "x") (N 5)) True 3
  , testCase "Boolean Expressions" $
    [LOAD "x",LOADI 5,JMPGE 3] @=? bcomp (And (Less (V "x") (N 5)) (Bc True)) False 3
  , testCase "Boolean Expressions" $
    [JMP 3,LOAD "x",LOADI 5,JMPLESS 3] @=? bcomp (And (Bc False) (Less (V "x") (N 5))) True 3
  , testCase "Boolean Expressions" $
    [JMP 6,LOAD "x",LOADI 5,JMPGE 3] @=? bcomp (And (Bc False) (Less (V "x") (N 5))) False 3
  , testCase "Commands" $
    [LOAD "u",LOADI 1,JMPGE 5,LOAD "u",LOADI 1,ADD,STORE "u",JMP 2,LOAD "u",STORE "v"] @=? ccomp (If (Less (V "u") (N 1)) (Assign "u" (Plus (V "u") (N 1))) (Assign "v" (V "u")))
  , testCase "Commands" $
    [LOAD "u",LOADI 1,JMPGE 5,LOAD "u",LOADI 1,ADD,STORE "u",JMP (-8)] @=? ccomp (While (Less (V "u") (N 1)) (Assign "u" (Plus (V "u") (N 1))))
  ]