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

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | DEF String [Cmd]
         | CALL String
         deriving Show

--Type Definition
type Stack = [Int]
type D = Maybe Stack -> Maybe Stack

--Semantic Definition
semCmd :: Cmd -> D
semCmd (LD i) (Just s) = Just (i : s)
semCmd ADD (Just s) = case length s of
        0 -> Nothing
        1 -> Nothing
        _ -> Just (sum(take 2 s) : drop 2 s)
semCmd MULT (Just s) = case length s of
        0 -> Nothing
        1 -> Nothing
        _ -> Just (product(take 2 s) : drop 2 s)
semCmd DUP (Just s) = case length s of
        0 -> Nothing
        _ -> Just ((head s) : s)

sem :: Prog -> D
sem [] (Just s) = (Just s)
sem (x:xs) (Just s) | (sem xs (semCmd x (Just s))) == Nothing = Nothing
                    | otherwise = (sem xs (semCmd x (Just s)))
sem _ _ = Nothing

--Must be used so that GHCi can show the [Int]
eval :: Prog -> Maybe Stack
eval pro = (sem pro (Just []))

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

{- Q2 Extending the Stack Language by Macros -}

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
