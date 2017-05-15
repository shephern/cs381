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

{-
data Type = Int | BBox | TypeError
            deriving (Eq, Show)

ok :: Type -> Bool
ok TypeError = False
ok _         = True
-}

type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox X = (1, 1)
bbox (TD x y) = (fst(bbox x), snd(bbox y)+1)
bbox (LR x y) = (fst(bbox x)+1, snd(bbox y))

s4 = TD X (TD X (TD X X))