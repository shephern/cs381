{-
Homework #1
Completed by Drake Seifert and Nathan Shepherd
seifertd, shephern
-}
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
Write a Mini Logo macro vector that draws a line from a given
position (x1,y1) to a given position (x2,y2) and represent the
macro in abstract syntax, that is, as a Haskell data type value.
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

--Example usage: ppCircuit halfAdder
ppCircuit :: Circuit -> IO () --Outputs IO to embed newlines
ppCircuit (Con g l) =putStr("Gates: \n"++show(g)++"Links:\n"++show(l))


{-
Q3 a
Represent the expression -(3+4)*7 in the alternative abstract
syntax.
-}

data Expr = N Int
	| Plus Expr Expr
	| Times Expr Expr
	| Neg Expr
	deriving Show

data Op = Add | Multiply | Negate
	deriving Show

data Exp = Num Int
	| Apply Op [Exp]
	deriving Show

q3a = Apply Multiply [Apply Negate [Apply Add [Num 3, Num 4]], Num 7]

{-
Q3 b
What are the advantages or disadvantages of either representation?

Personally, I feel like the first representation is easier to read
and understand, and flows a lot more like written math. The
second representation is a little more compact with the downside
of being a little more challenging to think about conceptually
when dealing with multiple operators. The second method however
can also take entire lists of numbers to an operator at once which
could be more useful for specific applications where an operator is
mapped across a large amount of operands. Whichever one is best
really depends on preference and the given scenario.
-}

{-
Q3 c
Define a function translate :: Expr -> Exp that translates
expressions given in the first abstract syntax into equivalent
expressions in the second abstract syntax.
-}

translate :: Expr -> Exp
translate (N n) = (Num n)
translate (Plus n m) = Apply Add [(translate n), (translate m)]
translate (Times n m) = Apply Multiply [(translate n), (translate m)]
translate (Neg n) = Apply Negate [translate n]
