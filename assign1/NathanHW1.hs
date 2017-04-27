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

--NOTE: Num defined above on line 12

--Define abstract syntax
data Circuit = Con Gates Links deriving Show

data Gates = GCon [(Num, GateFN)]

data GateFN = And
            | Or
            | Xor
            | Not

data Links = LCon [((Num, Num), (Num, Num))]

instance Show Links where
        show (LCon (((x1,y1),(x2,y2)):rest)) = "From "++show(x1)++"."++
                                          show(y1)++" to "++show(x2)++
                                          "."++show(y2)++"\n"
                                          ++show(LCon rest)
        show (LCon []) = ""

instance Show Gates where
        show (GCon ((n, gate):rest)) = "Gate "++show(n)++" of type "
                                       ++show(gate)++"\n"++show(GCon rest)
        show (GCon []) = ""

instance Show GateFN where
        show And = "And"
        show Or = "Or"
        show Xor = "Xor"
        show Not = "Not"

{-
Q2 b
Represent half adder circuit as Haskell Data type value
-}
halfAdder :: Circuit
halfAdder = Con (GCon [(1, Xor), (2, And)])
                         (LCon [((1,1),(2,1)), ((1,2),(2,2))])

{-
Q2 c
Print implicitly defined using Show for circuit
Print function below
-}

--Usage: ppCircuit halfAdder
ppCircuit :: Circuit -> IO () --Outputs IO to embed newlines
ppCircuit (Con g l) =putStr("Gates: \n"++show(g)++"Links:\n"++show(l))

