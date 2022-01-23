module Machine
(
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map as Map
import Data.Maybe

--TODO Task 1.1
type Vname = String
--TODO Task 1.2
type Val = Int
--TODO Task 1.3
type State = (Map.Map Vname Val)
--TODO Task 1.4
data Instr =
        LOADI Val | LOAD Vname | ADD | STORE Vname
         | JMP Int | JMPLESS Int |JMPGE Int
        deriving (Eq, Read, Show)
--TODO Task 1.5
type Stack = [Int]

type Config = (Int,State,Stack)
--TODO Task 1.7
iexec :: Instr -> Config -> Config
--iexec ((LOADI x) (Config(counter, state , stack))) = Config((counter + 1), state, push x stack )
iexec (LOADI value) (counter, state , stack) = (counter + 1, state, stack ++ [value] )
iexec (LOAD string) (counter, state , stack) = (counter + 1, state, stack ++ removeMaybe (Map.lookup string state))

iexec ADD (counter, state, stack) = (counter + 1, state, Prelude.take (length stack-2) stack ++ [stack !! (length stack-2) + stack !! (length stack-1)])

iexec (STORE v) (counter, state , stack) = (counter + 1, Map.insert v (last stack) state , Prelude.drop 1 stack )
iexec (JMP i) (counter, state , stack) = (counter + 1 + i, state, stack )
iexec (JMPLESS i) (counter, state, stack) =
        if stack !! (length stack-1) < stack !! (length stack-2) then (counter + 1 + i, state, Prelude.take (length stack-2) stack)
        else (counter + 1, state,Prelude.take (length stack-2) stack)
iexec (JMPGE i) (counter, state, stack) =
        if stack !! (length stack-1) >= stack !! (length stack-2) then (counter + 1 + i, state, Prelude.take (length stack-2) stack)
        else (counter + 1, state,Prelude.take (length stack-2) stack)
--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] (counter, state , stack) = (counter , state , stack)
exec xs (counter, state , stack) = exec (Prelude.drop 1 xs) (iexec (head xs) (counter, state, stack))
--exec xs (counter, state , stack) = 
--        | [] (counter, state , stack) = (counter , state , stack)
--        | otherwise = exec (Prelude.drop 1 xs) (iexec (head xs) (counter, state, stack))

removeMaybe :: Maybe Int -> [Int]
removeMaybe Nothing = []
removeMaybe (Just i) = [i]