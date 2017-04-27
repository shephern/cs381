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
--Example usage: R2 "Hello World"
instance Show Pos where
	show (R1 a) = show a
	show (R2 a) = show a

--Example usage: Pen Up
instance Show Mode where
	show Up = "!Up!"
	show Down = "!Down!"

{-
Q2 a
Define abstract syntax for Digital Circuit
as a Haskell data type
-} 

--Num defined above

--Define abstract syntax
data Circuit = Con Gates Links
            

data Gates = GCon [(Num, GateFN)]

data GateFN = And
            | Or
            | Xor
            | Not

data Links = LCon [[Num, Num, Num, Num]]

{-
Q2 b
Represent half adder circuit as Haskell Data type value
-}
HalfAdder = C (GCon [(1, Xor), (2, And)])
              ([[1,1,2,1], [1,2,2,2]])


