module P3 where

import System.IO

data Cmd = Pen Mode
         | MoveTo Int Int
         | Seq Cmd Cmd
data Mode = Up | Down deriving Eq --modes. Derives Eq so they can compare.

type State = (Mode,Int,Int)
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
semS :: Cmd -> State -> (State,Lines)
semS (Pen m) s = ((m,tsnd s,ttrd s),([]))
--Sets pen to Mode, doesn't move anything.
semS (MoveTo i  i') s | tfst s == Up = ((tfst s,i,i'),([]))
                      | otherwise = ((tfst s,i,i'),([(tsnd s,ttrd s,i,i')]))
--Creates a line if the pen's down, otherwise just move.
semS (Seq c c') s = semS c (fst (semS c' s))
--Runs two cmds.
--Only gets the second one's line.

sem' :: Cmd -> Lines
sem' cmd = snd (semS (cmd) (Up,0,0))



--Questions: What does sem' do?
--How do you do semS (Seq)?
