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
{-
bbox X = (1, 1)
bbox (TD x y) = (fst(bbox x)*fst(bbox y), snd(bbox x)+snd(bbox y))
bbox (LR x y) = (fst(bbox x)+fst(bbox y), snd(bbox y)*snd(bbox x))
-}

rect :: Shape -> Maybe BBox
rect shape = case fst(bbox shape)==snd(bbox shape) of
		True -> Just(bbox shape)
		False -> Nothing

s4 = TD X (TD X (TD X X

s5 = TD (LR X X) (TD X (LR X X))
s6 = TD X (TD X X)

--hollow square:
s7 = LR s5 s6