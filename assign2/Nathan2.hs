{-
   - Haskell Assignment 2
   - CS 381, SP 2017
   - changle, shephern, seifertd
-}
module Nathan2 where
{-
   - Ex. 1
   - Stack Language
   - Usage: eval Prog
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
        0 -> error("ADD ERROR-- Cannot perform on empty list")
        1 -> error("ADD ERROR-- Cannot perform on signleton list")
        _ -> (sum(take 2 s) : drop 2 s)
semCmd MULT s = case length s of
        0 -> error("MULT ERROR-- Cannot perform on empty list")
        1 -> error("MULT ERROR-- Cannot perform on singleton list")
        _ -> (product(take 2 s) : drop 2 s)
semCmd DUP s = case length s of
        0 -> error("DUP ERROR-- Cannot perform on empty list")
        _ -> ((head s) : s)

--Must be used so that GHCi can show the [Int]
eval :: Prog -> Stack
eval pro = sem pro []

--Defined test cases
p1::Prog
p1 = [LD 3,DUP,ADD,DUP,MULT]

p2::Prog
p2 = [LD 3,ADD]

p3::Prog
p3 = []

p4::Prog
p4 = [MULT, LD 2]

p5::Prog
p5 = [LD 3,DUP,ADD,ADD]

p6::Prog
p6 = [LD 4,DUP,DUP,ADD,MULT,LD 7,ADD]

{-
   - Ex. 2
   - 
-}


