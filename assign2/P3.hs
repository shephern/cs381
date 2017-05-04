module P3 where

--Used for drawing SVG lines.
import System.IO

data Cmdl = Pen Mode
         | MoveTo Int Int
         | Seq Cmdl Cmdl
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
semS :: Cmdl -> State -> (State,Lines)
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
