{-# language RecordWildCards #-}

module CodeWorld.Test.Relative (
  Components(..),
  RelativePicSpec(..),
  (===),
  toRelative,
  isSouthOf,
  isNorthOf,
  isWestOf,
  isEastOf,
  isSouthEastOf,
  isSouthWestOf,
  isNorthEastOf,
  isNorthWestOf,
  isBelow,
  isAbove,
  isLeftOf,
  isRightOf,
  atSamePosition,
  )where


import Data.List                        (sort)

import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Test.AbsTypes          (Position(..), fromPosition)
import CodeWorld.Test.Normalize (
  NormalizedPicture(..),
  getSubPictures,
  stripTranslation,
  getTranslation,
  couldHaveTranslation,
  contains,
  )



data DirectionV = South deriving (Eq,Ord,Show)
data DirectionH = West | East deriving (Eq,Ord,Show)

data Direction = Direction {
  vertical :: Maybe DirectionV,
  horizontal :: Maybe DirectionH
} deriving (Eq,Ord)


{- |
Abstract representation of spatial positioning between picture components.
-}
data RelativePicSpec
  = Is NormalizedPicture Direction NormalizedPicture
  | Alone NormalizedPicture
  deriving(Eq,Ord)


{- |
Abstract representation of a picture in terms of components and spatial positioning.
-}
newtype Components = Components (NormalizedPicture,[RelativePicSpec]) deriving (Eq,Ord,Show)


instance Show Direction where
  show Direction{..} = case (vertical, horizontal) of
    (Nothing, Nothing) -> "OnTop"
    (Nothing, Just a)  -> show a
    (Just a, Nothing)  -> show a
    (Just a, Just b)   -> show a ++ show b


instance Show RelativePicSpec where
  show (Is p1 dir p2) = show p1 ++ " is " ++ show dir ++ " of " ++ show p2
  show (Alone p) = show p



(===) :: NormalizedPicture -> NormalizedPicture -> Bool
p1 === p2 = toRelative p1 == toRelative p2


southOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec
southOf p1 = Is p1 (Direction (Just South) Nothing)


northOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec
northOf = flip southOf


westOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec
westOf p1 = Is p1 (Direction  Nothing (Just West))


eastOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec
eastOf = flip westOf


onTopOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec
onTopOf p1 = Is p1 (Direction Nothing Nothing)


southwestOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec
southwestOf p1 = Is p1 (Direction (Just South) (Just West))


southeastOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec
southeastOf p1 = Is p1 (Direction (Just South) (Just East))


northwestOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec
northwestOf = flip southeastOf


northeastOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec
northeastOf = flip southwestOf


alone :: NormalizedPicture -> RelativePicSpec
alone = Alone


containedSouthOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedSouthOf p q (Is p1 (Direction (Just South) Nothing) p2) = p1 `contains` p && p2 `contains` q
containedSouthOf _ _ _ = False


containedNorthOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedNorthOf p q (Is p1 (Direction (Just South) Nothing) p2) = p1 `contains` q && p2 `contains` p
containedNorthOf _ _ _ = False


containedWestOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedWestOf p q (Is p1 (Direction Nothing (Just West)) p2) = p1 `contains` p && p2 `contains` q
containedWestOf p q (Is p1 (Direction Nothing (Just East)) p2) = p1 `contains` q && p2 `contains` p
containedWestOf _ _ _ = False


containedEastOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedEastOf p q (Is p1 (Direction Nothing (Just East)) p2) = p1 `contains` p && p2 `contains` q
containedEastOf p q (Is p1 (Direction Nothing (Just West)) p2) = p1 `contains` q && p2 `contains` p
containedEastOf _ _ _ = False


containedSouthWestOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedSouthWestOf p q (Is p1 (Direction (Just South) (Just West)) p2) = p1 `contains` p && p2 `contains` q
containedSouthWestOf _ _ _ = False


containedSouthEastOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedSouthEastOf p q (Is p1 (Direction (Just South) (Just East)) p2) = p1 `contains` p && p2 `contains` q
containedSouthEastOf _ _ _ = False


containedNorthWestOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedNorthWestOf p q (Is p1 (Direction (Just South) (Just East)) p2) = p1 `contains` q && p2 `contains` p
containedNorthWestOf _ _ _ = False


containedNorthEastOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedNorthEastOf p q (Is p1 (Direction (Just South) (Just West)) p2) = p1 `contains` q && p2 `contains` p
containedNorthEastOf _ _ _ = False


containedAbove :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedAbove p q (Is p1 (Direction (Just South) _) p2) = p1 `contains` q && p2 `contains` p
containedAbove _ _ _ = False


containedBelow :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedBelow p q (Is p1 (Direction (Just South) _) p2) = p1 `contains` p && p2 `contains` q
containedBelow _ _ _ = False


containedLeftOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedLeftOf p q (Is p1 (Direction _ (Just West)) p2) = p1 `contains` p && p2 `contains` q
containedLeftOf p q (Is p1 (Direction _ (Just East)) p2) = p1 `contains` q && p2 `contains` p
containedLeftOf _ _ _ = False


containedRightOf :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedRightOf p q (Is p1 (Direction _ (Just East)) p2) = p1 `contains` p && p2 `contains` q
containedRightOf p q (Is p1 (Direction _ (Just West)) p2) = p1 `contains` q && p2 `contains` p
containedRightOf _ _ _ = False


containedSameSpot :: NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool
containedSameSpot p q (Is p1 (Direction Nothing Nothing) p2) = p1 `contains` p && p2 `contains` q || p1 `contains` q && p2 `contains` p
containedSameSpot _ _ _ = False


{- |
True if the first argument is below the second and aligned on the X-axis.
-}
isSouthOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isSouthOf = compositeRelation containedSouthOf


{- |
True if the first argument is above the second and aligned on the X-axis.
-}
isNorthOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isNorthOf = compositeRelation containedNorthOf


{- |
True if the first argument is left of the second and aligned on the Y-axis.
-}
isWestOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isWestOf = compositeRelation containedWestOf


{- |
True if the first argument is right of the second and aligned on the Y-axis.
-}
isEastOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isEastOf = compositeRelation containedEastOf


{- |
True if the first argument is below and to the left of the second.
-}
isSouthWestOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isSouthWestOf = compositeRelation containedSouthWestOf


{- |
True if the first argument is below and to the right of the second.
-}
isSouthEastOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isSouthEastOf = compositeRelation containedSouthEastOf


{- |
True if the first argument is above and to the left of the second.
-}
isNorthWestOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isNorthWestOf = compositeRelation containedNorthWestOf


{- |
True if the first argument is above and to the right of the second.
-}
isNorthEastOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isNorthEastOf = compositeRelation containedNorthEastOf


{- |
True if the first argument is above the second, ignoring horizontal positioning.
-}
isAbove :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isAbove = compositeRelation containedAbove


{- |
True if the first argument is below the second, ignoring horizontal positioning.
-}
isBelow :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isBelow = compositeRelation containedBelow


{- |
True if the first argument is left of the second, ignoring vertical positioning.
-}
isLeftOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isLeftOf = compositeRelation containedLeftOf


{- |
True if the first argument is right of the second, ignoring vertical positioning.
-}
isRightOf :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
isRightOf = compositeRelation containedRightOf


{- |
True if the first argument is at the same position as the second.
-}
atSamePosition :: NormalizedPicture -> NormalizedPicture -> [RelativePicSpec] -> Bool
atSamePosition = compositeRelation containedSameSpot


compositeRelation
  :: (NormalizedPicture -> NormalizedPicture -> RelativePicSpec -> Bool)
  -> NormalizedPicture
  -> NormalizedPicture
  -> [RelativePicSpec]
  -> Bool
compositeRelation g p q rs = all (`any` rs) allRelations
   where
      allRelations = [g x y | x <- getSubPictures p, y <- getSubPictures q]


toRelative :: NormalizedPicture -> Components
toRelative p = case p of
  Pictures ps -> Components (Pictures $ map stripTranslation ps, sort $ relativePosition ps)
  a           -> let noTranslation = stripTranslation a in
    Components (noTranslation,[alone noTranslation])


relativePosition :: [NormalizedPicture] -> [RelativePicSpec]
relativePosition [] = []
relativePosition (p:ps)
  | couldHaveTranslation p = othersTrans ++ relativePosition ps
  | otherwise = others ++ relativePosition ps
  where
    (pX,pY) = getTranslation p

    asCenter pic = let (bX,bY) = getTranslation pic in
      translated (fromPosition $ bX-pX) (fromPosition $ bY-pY) $ stripTranslation pic

    othersTrans = map (\pic ->
        orientation (asCenter pic) (stripTranslation p) $ stripTranslation pic
        )
      ps

    others = map (\pic -> orientation pic p $ stripTranslation pic) ps


orientation
  :: NormalizedPicture
  -> ( NormalizedPicture
    -> NormalizedPicture
    -> RelativePicSpec
     )
orientation = toDirection . getTranslation
  where
    toDirection (a,b) = case (a,b) of
      (Zero , Neg _) -> northOf
      (Zero , Pos _) -> southOf
      (Neg _, Zero ) -> eastOf
      (Pos _, Zero ) -> westOf
      (Pos _, Pos _) -> southwestOf
      (Pos _, Neg _) -> northwestOf
      (Neg _, Pos _) -> southeastOf
      (Neg _, Neg _) -> northeastOf
      (Zero , Zero ) -> onTopOf
