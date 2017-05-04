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
--macroList = ["myMacro"] --Check if macros exist here
{-
checkMacroExists :: String -> SavedMacros -> Bool
checkMacroExists str arr = elem str arr 
-}
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | DEF String [Cmd]
         | CALL String
         deriving Show

--Type Definition
type Stack = [Int]

------------------------------------------------------------------------------------
type Macros = [(String,Prog)]

type State = (Macros, Stack)

type S = State -> Maybe State

semCmd2 :: Cmd -> S
semCmd2 (LD i) s = Just (fst(s),i:snd(s))
semCmd2 (ADD) s = Just (fst(s), (head(snd(s)) + head(tail(snd(s))) : tail(tail(snd(s)))))
--semCmd2 (ADD) (_) = Nothing
semCmd2 (MULT) s = Just (fst(s), (head(snd(s)) * head(tail(snd(s))) : tail(tail(snd(s)))))
--semCmd2 (MULT) (_) = Nothing
semCmd2 (DUP) s = Just (fst(s), head(snd(s)) : snd(s))
--semCmd2 (DUP) (_) = Nothing
semCmd2 (DEF w p) s = Just (((w,p) : fst(s)), snd(s))
--semCmd2 (CALL w) s = Just (sem2 (lookup( w fst(s)) s))

sem2 :: Prog -> S
sem2 [] s = Just(s)
------------------------------------------------------------------------------------

{-
--Usage: macroCMD (DEF "SQR" [DUP, MULT])
macroCMD :: Cmd -> SavedMacros
macroCMD (DEF str cmdSeq) = str : macroList
-}

type D = Stack -> Maybe Stack

--Semantic Definition

sem :: Prog -> D
sem [] s = (Just s)
--sem (x:xs) s = sem xs (semCmd x s)

semCmd :: Cmd -> D
semCmd (LD i) s = Just (i : s)
semCmd ADD s = case length s of
        0 -> Nothing
        1 -> Nothing
        _ -> Just (sum(take 2 s) : drop 2 s)
semCmd MULT s = case length s of
        0 -> Nothing
        1 -> Nothing
        _ -> Just (product(take 2 s) : drop 2 s)
semCmd DUP s = case length s of
        0 -> Nothing
        _ -> Just ((head s) : s)

--Must be used so that GHCi can show the [Int]
eval :: Prog -> Maybe Stack
eval pro = (sem pro [])

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

