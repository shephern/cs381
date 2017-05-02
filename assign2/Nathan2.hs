{-
   - Haskell Assignment 2
   - CS 381, SP 2017
   - changle, shephern, seifertd
-}

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

type D = Stack
       | Error

sem :: Prog -> D
     sem [] = Error

semCmd :: Cmd -> D
     semCmd (LD i) = i : Stack
