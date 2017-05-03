{-
   - Haskell Assignment 2
   - CS 381, SP 2017
   - changle, shephern, seifertd
-}
module Nathan2 where
{-
   - Ex. 1
   - Stack Language
-}

--Abstract syntax
type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP

--Type Definition
type Stack = [Int]

type D = Maybe Stack -> Stack

--Semantic Definition

sem :: Prog -> D
sem [] s = s
sem (x:xs) s = sem xs (semCmd x s)

semCmd :: Cmd -> Maybe D
semCmd (LD i) s = Just (i : s)
semCmd ADD s 
        | length s >= 2 = Just (sum(take 2 s) : s)
        | otherwise     = Nothing
semCmd MULT s 
        | length s >= 2 = Just (product(take 2 s) : s
        | otherwise     = Nothing
semCmd DUP s 
        | length s >= 1 = Just ((head s) : s)
        | otherwise     = Nothing

p::Prog
p = [LD 3,DUP,ADD,DUP,MULT]

