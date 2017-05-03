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
Stack s = []

sem :: Prog -> D
     sem [] d = d
     sem (x:xs) d = sem xs (semCmd x d)

semCmd :: Cmd -> D
     semCmd (LD i d) = i : d
     semCmd (ADD d) = sum(take 2 d) : d
     semCmd (MULT d) = product(take 2 d) : d
     semCmd (DUP d) = (head d) : d
