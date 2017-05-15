module Part1 where
{- Q1 - A Stack Language-Type Error -}

--Abstract syntax
type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC
         | SWAP
         | POP Int
         deriving Show

--Type Definition
type Stack = [Int]
type D = Maybe Stack -> Maybe Stack

--New Type Definitions
type Rank = Int
type CmdRank = (Int, Int)

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
semCmd INC (Just s) = case length s of
        0 -> Nothing
        _ -> Just ((head s + 1) : tail s)
semCmd SWAP (Just s) = case length s of
        0 -> Nothing
        1 -> Nothing
        _ -> Just (head (tail s) : head s : drop 2 s)
semCmd (POP i) (Just s) | (length s) < i = Nothing
                        | otherwise = Just (drop i s)
semCmd _ Nothing = Nothing

sem :: Prog -> D
sem [] (Just s) = (Just s)
sem (x:xs) (Just s) | (sem xs (semCmd x (Just s))) == Nothing = Nothing
                    | otherwise = (sem xs (semCmd x (Just s)))
sem _ _ = Nothing

--Must be used so that GHCi can show the [Int]
eval :: Prog -> Maybe Stack
eval pro = (sem pro (Just []))

--Rank Functionality
rankC :: Cmd -> CmdRank
rankC (LD _) = (0, 1)
rankC ADD = (2, 1)
rankC MULT = (2, 1)
rankC DUP = (1, 2)
rankC INC = (1, 1)
rankC SWAP = (2, 2)
rankC (POP i) = (i, 0)

rankP :: Prog -> Maybe Rank
rankP pro = rank pro (Just 0)

rank :: Prog -> Maybe Rank -> Maybe Rank
rank [] (Just r) = Just r
rank (x:xs) (Just r) | (Just r) < Just(fst(rankC x)) = Nothing
        | otherwise  = rank xs (Just(r - fst(rankC x) + snd(rankC x)))
rank _ _ = Nothing

typeChecker :: Prog -> Bool
typeChecker p = (rankP p /= Nothing)


semStatTC :: Prog -> Maybe Stack
semStatTC p | typeChecker p = (sem p (Just []))
            | otherwise = Nothing

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
p5 = [LD 4,DUP,DUP,LD 2,SWAP,INC]

p6::Prog
p6 = [LD 4,DUP,DUP,POP 3]
