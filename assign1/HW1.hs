module HW1 where

{-
Q1 a
Define the abstract syntax for Mini Logo as a Haskell
data type
-}

type Num = Int
type Name = String

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

data Pars = [Name]

data Vals = [Num]
