module HW3 where
{- Assignment #3
 - Completed by changle, shephern, seifertd
 - Part 1: shephern
 - Part 2: seifertd
 - Part 3: changle
 -}
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

{-
Q2 a
-}

data Shape = X 
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type Pixel = (Int,Int)
type Image = [Pixel]

sem2 :: Shape -> Image 
sem2 X           = [(1,1)]
sem2 (LR s1 s2) = d1 ++ [(x+maxx d1,y) | (x,y) <- sem2 s2] 
                 where d1 = sem2 s1
sem2 (TD s1 s2) = d2 ++ [(x,y+maxy d2) | (x,y) <- sem2 s1] 
                 where d2 = sem2 s2

maxx :: [Pixel] -> Int
maxx = maximum . map fst

maxy :: [Pixel] -> Int
maxy = maximum . map snd

s1 = LR (TD X X) X
s2 = TD X (LR X X)
s3 = TD (LR X X) X

type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox shape = (maximum (map fst (sem2 shape)), maximum (map snd (sem2 shape)))

{-
Q2 b
-}

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

checkFirst :: Shape -> Int -> Int -> Bool
checkFirst shape 1 _ = True
checkFirst shape x y
	| (count x (map fst(sem2 shape)) == y) == True = checkFirst shape (x-1) y
	| otherwise = False

checkSecond :: Shape -> Int -> Int -> Bool
checkSecond shape 1 _ = True
checkSecond shape x y
	| (count x (map snd(sem2 shape)) == y) == True = checkSecond shape (x-1) y
	| otherwise = False

rect :: Shape -> Maybe BBox
rect X = Just(1,1)
rect shape
	| (checkFirst shape (fst(bbox shape)) (snd(bbox shape)) ) && (checkSecond shape (snd(bbox shape)) (fst(bbox shape)) ) == True = Just(bbox shape)
	| otherwise = Nothing

--rectangle
s4 = TD X (TD X (TD X X))

s5 = TD (LR X X) (TD X (LR X X))
s6 = TD X (TD X X)

--hollow square:
s7 = LR s5 s6

--square
s8 = TD (LR X X) (LR X X)

{-
Q3
-}

--a)
--1) f and g both return a list, though it is unknown what type.
--    f's header would be [a]->a->[a]
--    g's header would be [a]->b->[b]
--2) For f, null works on a list, so x must be a list, and it returns x if x is
--    not empty. If x is empty, it returns y in a list, so x and y must be the
--    same data type for it to be valid in strong typing.
--   g returns a list both times, but it never returns x, so the type of x
--    isn't important, and can be a different variable.
--3) g, since the type of list is dependent on only y, whereas in f, x and y
--    need to be of the same type.
--4) f returns x as an output, so y must also be of the type x but not in a
--    list, while g only returns an empty list or y in a list, so y does not
--    have to be the same type as x.
--
--b) You could just take head [b] + snd (head [(a,b)]) and return it.
h :: [b] -> [(a, b)] -> [b]
h x y = x ++ snd (unzip y)

--c) You could take the result of (a -> b) and append it to
--    the result of ((a -> b) -> a) as a tuple, then return fst (b,a)
k :: (a -> b) -> ((a -> b) -> a) -> b
k f g = f (g f)

--d) No, it's difficult because the definition is too broad, and you don't have
--    a b to already work with. If you knew what b was to begin with, it's not
--    a -> b, it'd be a -> DataType. It's impossible to create a function that
--    will always output the correct data type b given only a, especially since
--    a might want a different b with the same a type (So 1 might want "b", but
--    2 might want True).
--Partial Examples that don't work:
--s :: a -> b
--s a = null a --Not quite a->b, it's a->Bool.
--s a = show a --a -> String
--s a = read "a" :: Int --a -> Int, assuming a is an int.