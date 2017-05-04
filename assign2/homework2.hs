module Homework2 where
import Data.Maybe
--Used for drawing SVG lines.
import System.IO
{-
   - Haskell Assignment 2
   - CS 381, SP 2017
   - Le-Chuan Justin, Drake Seifert, Nathan Shepherd
   - changle, seifertd, shephern
   - Part 1: Done by shephern, seifertd
   - Part 2: Done by seifertd
   - Part 3: Done by changle
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
--type S = Maybe State -> Maybe State

semCmd2 :: Cmd -> S
semCmd2 (LD i) s = Just (fst(s),i:snd(s))
semCmd2 (ADD) s = case length (snd(s)) of
        0 -> Nothing
        1 -> Nothing
        _ -> Just (fst(s),
                (head(snd(s)) + head(tail(snd(s))) : tail(tail(snd(s)))))
semCmd2 (MULT) s = case length (snd(s)) of
        0 -> Nothing
        1 -> Nothing
        _ -> Just (fst(s),
                (head(snd(s)) * head(tail(snd(s))) : tail(tail(snd(s)))))
semCmd2 (DUP) s = case length (snd(s)) of
        0 -> Nothing
        _ -> Just (fst(s), head(snd(s)) : snd(s))
semCmd2 (DEF w p) s = Just (((w,p) : fst(s)), snd(s))
--semCmd2 (CALL w) (Just s) = case (lookup w (fst(s))) of
--        Nothing -> Nothing
--        (Just p) -> Just (fst(s), fromJust(sem p (snd(s))))

sem2 :: Prog -> S
sem2 [] s = (Just s)
--sem2 (x:xs) (Just s) = (sem2 xs (semCmd2 x (Just s)))
{-sem2 (x:xs) (Just s) | (sem2 xs (semCmd2 x (Just s))) == Nothing = Nothing
                     | otherwise = (sem2 xs (semCmd2 x (Just s)))
sem2 _ _ = Nothing -}

p7::Prog
p7 = [LD 4, DEF "SQR" [DUP,MULT], CALL "SQR"]

p8::Prog
p8 = [LD 5, DEF "CUBE" [DUP,MULT,DUP,MULT], CALL "CUBE"]

s1::State
s1 = ([("SQR", [DUP,MULT])], [1,2,3,4])




data Cmdl = Pen Mode
         | MoveTo Int Int
         | Seq Cmdl Cmdl
data Mode = Up | Down deriving Eq --modes. Derives Eq so they can compare.

type Statel = (Mode,Int,Int)
--pen mode, x pos, y pos
type Line  = (Int,Int,Int,Int)
--init x, init y, final x, final y
type Lines = [Line] --lines drawn
--a list of lines drawn

tfst :: (a,b,c) -> a
tfst (a,_,_) = a

tsnd :: (a,b,c) -> b
tsnd (_,b,_) = b

ttrd :: (a,b,c) -> c
ttrd (_,_,c) = c

--((state),(lines))
--((Mode,Int,Int),([(Int,Int,Int,Int)]))
semS :: Cmdl -> Statel -> (Statel,Lines)
semS (Pen m) s = ((m,tsnd s,ttrd s),([]))
--Sets pen to Mode, doesn't move anything.
semS (MoveTo i  i') s | tfst s == Up = ((tfst s,i,i'),([]))
                      | otherwise = ((tfst s,i,i'),([(tsnd s,ttrd s,i,i')]))
--Creates a line if the pen's down, otherwise just move.
semS (Seq c c') s = let (s',l) = semS c s
                        (s'',l') = semS c' s'
                  in (s'',l++l')
--Runs two Cmdls.

sem' :: Cmdl -> Lines
sem' cmdl = snd (semS (cmdl) (Up,0,0))


--Below this line is copied from SVG.hs

ppLines :: Lines -> IO ()
ppLines ls = do h <- openFile "MiniLogo.svg" WriteMode
                hPutStr h (svgHdr++concatMap ppLine ls++svgFtr)
                hClose h

                  -- fixed size and maginifaction factor
-- (can be generalized easily)
--
factor=100
yMax=1100

svgHdr = "<?xml version=\"1.0\" standalone=\"no\"?>\n \
         \ <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n \
         \    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n \
         \ <svg width=\"12cm\" height=\"11cm\" viewBox=\"0 0 1200 1100\"\n \
         \    xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n \
         \ <title>Mini Logo Result Viewer</title>\n \
         \ <desc>A set of line segments</desc>\n \
         \ <rect x=\"10\" y=\"10\" width=\"1180\" height=\"1080\" \
         \       fill=\"none\" stroke=\"red\" /> "
svgFtr = "</svg>\n"

ppLine :: Line -> String
ppLine (x,y,x',y') = "<path d=\"M "++ppPos x y++" L "++ppPos x' y'++"\" "++
                     "stroke=\"blue\" stroke-width=\"5\" />\n"

ppPos :: Int -> Int -> String
ppPos x y = show (50+factor*x)++" "++show (yMax-50-factor*y)
