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

type D = Stack -> Stack

--Semantic Definition

sem :: Prog -> D
sem [] s = s
sem (x:xs) s = sem xs (semCmd x s)

semCmd :: Cmd -> D
semCmd (LD i) s = (i : s)
semCmd ADD s = case length s of
        0 -> error("ADD ERROR")
        1 -> error("ADD ERROR")
        _ -> (sum(take 2 s) : drop 2 s)
semCmd MULT s = case length s of
        0 -> error("MULT ERROR")
        1 -> error("MULT ERROR")
        _ -> (product(take 2 s) : drop 2 s)
semCmd DUP s = case length s of
        0 -> error("DUP ERROR")
        _ -> ((head s) : s)

--Must be used so that GHCi can show the [Int]
eval :: Prog -> Stack
eval pro = sem pro []

p1::Prog
p1 = [LD 3,DUP,ADD,DUP,MULT]

p2::Prog
p2 = [LD 3,ADD]

p3::Prog
p3 = []


