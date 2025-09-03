{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}

module CodeWorld.Tasks.Types (
  TextStyle(..),
  Font(..),
  ) where


import Control.DeepSeq                  (NFData)
import Data.Data                        (Data)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)



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
