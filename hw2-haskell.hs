--Group: Jeffry Robbins, April Child
--Tasks in group:
--1 a)April 
--1 b)April
--2 a)Jeffry
--2 b)Jeffry
----------------------------------------------------------------------------------------------------------------------------------
--import Graphics.HGL.Draw.Pen
import Data.Text
import Prelude(Eq, Int, Show, Char, (+), (-))

--a)syntax for mini logo
data Cmd = Pen Mode
        | MovePos Position Position
        | Def [Char] Parse Cmd
        | Call [Char] Values
        | SemiColon Cmd Cmd
        deriving (Eq,Show)

data Mode = Up|Down
            deriving (Eq,Show)
data Position = PositionA Int|PositionB [Char]
            deriving (Eq,Show)
data Parse = ParseL [Char] Parse|ParseR [Char]
            deriving (Eq,Show)
data Values = ValueA Int Values|ValueB Int
            deriving (Eq,Show)

--b)Vector "Macros", first parse, then move

vector :: Cmd
vector = Def "vector" (ParseL "x-a"(ParseL "y-a"(ParseL "x-b"(ParseR "y-b")))) (SemiColon(MovePos (PositionB "x-a")(PositionB "y-a")) (SemiColon(Pen Down)(MovePos (PositionB "x-b")(PositionB "y-b"))))

--c) create steps, not using vector, move y up, then move x to the side
steps :: Int -> Cmd
steps x = SemiColon(MovePos(PositionA 0)(PositionA 0)) (SemiColon (Pen Down)(calcsteps x 0))

calcsteps :: Int -> Int -> Cmd
calcsteps 0 y = Pen Up 
calcsteps x y = SemiColon(MovePos(PositionA y)(PositionA (y+1))) (SemiColon(MovePos(PositionA(y+1))(PositionA(y+1)))(calcsteps(x-1)(y+1)))