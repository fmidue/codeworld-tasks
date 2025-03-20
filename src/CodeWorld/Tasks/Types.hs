
module CodeWorld.Tasks.Types (
  Point,
  Vector,

  Color,
  TextStyle,
  Font,

  red,
  green,
  yellow,
  ) where


import Data.Text                        (Text)



--type NodeId = Int
type Point = (Double,Double)
type Vector = (Double,Double)

data TextStyle
  = Plain
  | Bold
  | Italic
  deriving (Eq,Ord,Show)


data Font
  = SansSerif
  | Serif
  | Monospace
  | Handwriting
  | Fancy
  | NamedFont Text
  deriving (Eq,Ord,Show)


data Color
  = Yellow
  | Green
  | Red
  deriving (Eq,Ord,Show)


green, red, yellow :: Color
yellow = Yellow
green  = Green
red    = Red
