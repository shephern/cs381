module HW1 where

{-
Q1 a
Define the abstract syntax for Mini Logo as a Haskell data type
-}

--Disregard built in name "Num"
import Prelude hiding (Num)

--Set data types
type Num = Int
type Name = String

--Define abstract syntax
data Cmd = Pen Mode
	| Moveto Pos Pos
	| Def Name Pars Cmd
	| Call Name Vals
	| Seq [Cmd]
	deriving Show

data Mode = Down
	| Up

data Pos = R1 Num
	| R2 Name

type Pars = [Name]
type Vals = [Num]

--Example usage: R1 42
--Example usage: R2 "Hello World"
instance Show Pos where
	show (R1 a) = show a
	show (R2 a) = show a

--Example usage: Pen Up
instance Show Mode where
	show Up = "!Up!"
	show Down = "!Down!"

{-
Q1 b
Write a Mini Logo macro vector that draws a line from a given position 
(x1,y1) to a given position (x2,y2) and represent the macro in abstract
syntax, that is, as a Haskell data type value.
-}

vector = Def "vector"
	["x1", "y1", "x2", "y2"]
	(Seq [
		Pen Up,
		Moveto (R2 "x1") (R2 "y1"),
		Pen Down,
		Moveto (R2 "x2") (R2 "y2"),
		Pen Up
		])

{-
Q1 c
Define a Haskell function steps :: Int -> Cmd that constructs a Mini
Logo program which draws a stair of n steps.
-}

steps :: Int -> Cmd
--steps n <= 0 = Call "Invalid" [0, 0, 0, 0]
steps 0 = Call "vector" [0, 0, 0, 0]
steps 1 = 	Seq [
					Call "vector" [0, 0, 0, 0],
					Call "vector" [0, 0, 0, 1],
					Call "vector" [0, 1, 1, 1]
					]
steps n = 			Seq [
					steps (n-1),
					Seq [Call "vector" [n-1, n-1, n-1, n],
						 Call "vector" [n-1, n, n, n]
						]
					]