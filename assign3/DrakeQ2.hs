module DrakeQ2 where

{-
Q2 a
Define a type checker for the shape language as a Haskell function
-}

data Shape = X 
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type Pixel = (Int,Int)
type Image = [Pixel]

sem :: Shape -> Image 
sem X           = [(1,1)]
sem (LR s1 s2) = d1 ++ [(x+maxx d1,y) | (x,y) <- sem s2] 
                 where d1 = sem s1
sem (TD s1 s2) = d2 ++ [(x,y+maxy d2) | (x,y) <- sem s1] 
                 where d2 = sem s2

maxx :: [Pixel] -> Int
maxx = maximum . map fst

maxy :: [Pixel] -> Int
maxy = maximum . map snd

s1 = LR (TD X X) X
p1 = sem s1

s2 = TD X (LR X X)
p2 = sem s2

s3 = TD (LR X X) X
p3 = sem s3

type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox shape = (maximum (map fst (sem shape)), maximum (map snd (sem shape)))

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

checkFirst :: Shape -> Int -> Int -> Bool
checkFirst shape 1 _ = True
checkFirst shape x y
	| (count x (map fst(sem shape)) == y) == True = checkFirst shape (x-1) y
	| otherwise = False

checkSecond :: Shape -> Int -> Int -> Bool
checkSecond shape 1 _ = True
checkSecond shape x y
	| (count x (map snd(sem shape)) == y) == True = checkSecond shape (x-1) y
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