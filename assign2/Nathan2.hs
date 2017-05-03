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
semCmd ADD s = (sum(take 2 s) : s)
semCmd MULT s = (product(take 2 s) : s)
semCmd DUP s = ((head s) : s)


