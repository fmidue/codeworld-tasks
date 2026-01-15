{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}

module CodeWorld.Tasks.Types (
  TextStyle(..),
  Font(..),
  ReifyPicture(..),
  Shape(..),
  Style(..),
  ) where


import Control.DeepSeq                  (NFData)
import Data.Data                        (Data)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)

import CodeWorld.Tasks.Color            (Color)
import CodeWorld.Tasks.VectorSpace      (Point)


{-|
Basic syntax tree type of the CodeWorld API.
Each API function has a corresponding constructor.

The type variable is necessary for CSE detection via [Reify](https://hackage.haskell.org/package/data-reify).
The method replaces subexpressions with number ids, so we need to be flexible in the wrapped type.
-}
data ReifyPicture a
  = Rectangle Style Double Double
  | Circle Style Double
  | Polyline Shape [Point]
  | Arc Style Double Double Double
  | Curve Shape [Point]
  | Lettering Text
  | StyledLettering TextStyle Font Text
  | Color Color a
  | Translate Double Double a
  | Scale Double Double a
  | Dilate Double a
  | Rotate Double a
  | Reflect Double a
  | Clip Double Double a
  | Pictures [a]
  | And a a
  | CoordinatePlane
  | Logo
  | Blank
  deriving (Show, Foldable, Eq, Ord, Generic, NFData, Data)


data Style = Outline (Maybe Double) | Solid deriving (Show, Eq, Ord, Generic, NFData, Data)
data Shape = Closed Style | Open (Maybe Double) deriving (Show, Eq, Ord, Generic, NFData, Data)

{-|
Font modifier type used for stylized message rendering.
-}
data TextStyle
  = Plain
  | Bold
  | Italic
  deriving (Eq,Ord,Show,Generic,NFData,Data)


{-|
Text font type used for stylized message rendering.
-}
data Font
  = SansSerif
  | Serif
  | Monospace
  | Handwriting
  | Fancy
  | NamedFont Text
  deriving (Eq,Ord,Show,Generic,NFData,Data)
