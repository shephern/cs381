module HW1 where

{-
Q1 a
Define the abstract syntax for Mini Logo as a Haskell
data type
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

data Mode = Up
	| Down

data Pos = R1 Num
	| R2 Name

type Pars = [Name]
type Vals = [Num]

--Example usage: R1 3
instance Show Pos where
	show (R1 a) = show a
	--Not sure how to get this to work
	--show (R2 a) = show a

--Example usage: Pen Up
instance Show Mode where
	show Up = "!Up!"
	show Down = "!Down!"