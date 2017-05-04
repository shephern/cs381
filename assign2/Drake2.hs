module Drake2 where

{-
   - Haskell Assignment 2
   - CS 381, SP 2017
   - changle, shephern, seifertd
-}

{- Q1 - A Stack Language -}

--Abstract syntax
type Prog = [Cmd]
type SavedMacros = [String]
macroList = ["myMacro"] --Check if macros exist here

checkMacroExists :: String -> SavedMacros -> Bool
checkMacroExists str arr = elem str arr 

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | DEF String [Cmd]
         | CALL String
         deriving Show

--Usage: macroCMD (DEF "SQR" [DUP, MULT])
macroCMD :: Cmd -> SavedMacros
macroCMD (DEF str cmdSeq) = str : macroList

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
Q2 a
Extend the abstract syntax to represent macro definitions and calls,
that is, give a correspondingly changed data definition for Cmd.
-}



{-
Q2 b
Define a new type State to represent the state for the new language.
The state includes the macro definitions and the stack. Please note
that a macro definition can be represented by a pair whose first
component is the macro name and the second component is the sequence
of commands. Multiple macro definitions can be stored in a list. A
type to represent macro definitions could thus be defined as follows.
type Macros = [(String,Prog)]
-}



{-
Q2 c
Define the semantics for the extended language as a function sem2. As
in exercise 1, you probably want to define an auxiliary function
semCmd2 for the semantics of individual operations.
-}

