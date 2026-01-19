{-# language RecordWildCards #-}

module CodeWorld.Test.Relative (
  Components(..),
  RelativePicSpec(..),
  SpatialQuery,
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
import CodeWorld.Test.Abstract (
  AbstractPicture(..),
  getSubPictures,
  stripTranslation,
  getTranslation,
  couldHaveTranslation,
  contains,
  )


{- |
Alias for queries on spatial relationships.
-}
type SpatialQuery = [RelativePicSpec] -> Bool

data DirectionV = South deriving (Eq,Ord,Show)
data DirectionH = West | East deriving (Eq,Ord,Show)

data Direction = Direction {
  vertical :: Maybe DirectionV,
  horizontal :: Maybe DirectionH
} deriving (Eq,Ord)


{- |
Abstract representation of spatial positioning between two picture components.
-}
data RelativePicSpec
  = Is AbstractPicture Direction AbstractPicture
  | Alone AbstractPicture
  deriving(Eq,Ord)


{- |
Abstract representation of a picture in terms of its components
and the pairwise spatial positioning between them.
-}
newtype Components = Components (AbstractPicture,[RelativePicSpec]) deriving (Eq,Ord,Show)


instance Show Direction where
  show Direction{..} = case (vertical, horizontal) of
    (Nothing, Nothing) -> "OnTop"
    (Nothing, Just a)  -> show a
    (Just a, Nothing)  -> show a
    (Just a, Just b)   -> show a ++ show b


instance Show RelativePicSpec where
  show (Is p1 dir p2) = show p1 ++ " is " ++ show dir ++ " of " ++ show p2
  show (Alone p) = show p



(===) :: AbstractPicture -> AbstractPicture -> Bool
p1 === p2 = toRelative p1 == toRelative p2


southOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec
southOf p1 = Is p1 (Direction (Just South) Nothing)


northOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec
northOf = flip southOf


westOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec
westOf p1 = Is p1 (Direction  Nothing (Just West))


eastOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec
eastOf = flip westOf


onTopOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec
onTopOf p1 = Is p1 (Direction Nothing Nothing)


southwestOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec
southwestOf p1 = Is p1 (Direction (Just South) (Just West))


southeastOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec
southeastOf p1 = Is p1 (Direction (Just South) (Just East))


northwestOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec
northwestOf = flip southeastOf


northeastOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec
northeastOf = flip southwestOf


alone :: AbstractPicture -> RelativePicSpec
alone = Alone


containedSouthOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedSouthOf p q (Is p1 (Direction (Just South) Nothing) p2) =
  p1 `contains` p && p2 `contains` q
containedSouthOf _ _ _ = False


containedNorthOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedNorthOf p q (Is p1 (Direction (Just South) Nothing) p2) =
  p1 `contains` q && p2 `contains` p
containedNorthOf _ _ _ = False


containedWestOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedWestOf p q (Is p1 (Direction Nothing (Just West)) p2) =
  p1 `contains` p && p2 `contains` q
containedWestOf p q (Is p1 (Direction Nothing (Just East)) p2) =
  p1 `contains` q && p2 `contains` p
containedWestOf _ _ _ = False


containedEastOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedEastOf p q (Is p1 (Direction Nothing (Just East)) p2) =
  p1 `contains` p && p2 `contains` q
containedEastOf p q (Is p1 (Direction Nothing (Just West)) p2) =
  p1 `contains` q && p2 `contains` p
containedEastOf _ _ _ = False


containedSouthWestOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedSouthWestOf p q (Is p1 (Direction (Just South) (Just West)) p2) =
  p1 `contains` p && p2 `contains` q
containedSouthWestOf _ _ _ = False


containedSouthEastOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedSouthEastOf p q (Is p1 (Direction (Just South) (Just East)) p2) =
  p1 `contains` p && p2 `contains` q
containedSouthEastOf _ _ _ = False


containedNorthWestOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedNorthWestOf p q (Is p1 (Direction (Just South) (Just East)) p2) =
  p1 `contains` q && p2 `contains` p
containedNorthWestOf _ _ _ = False


containedNorthEastOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedNorthEastOf p q (Is p1 (Direction (Just South) (Just West)) p2) =
  p1 `contains` q && p2 `contains` p
containedNorthEastOf _ _ _ = False


containedAbove :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedAbove p q (Is p1 (Direction (Just South) _) p2) =
  p1 `contains` q && p2 `contains` p
containedAbove _ _ _ = False


containedBelow :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedBelow p q (Is p1 (Direction (Just South) _) p2) =
  p1 `contains` p && p2 `contains` q
containedBelow _ _ _ = False


containedLeftOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedLeftOf p q (Is p1 (Direction _ (Just West)) p2) =
  p1 `contains` p && p2 `contains` q
containedLeftOf p q (Is p1 (Direction _ (Just East)) p2) =
  p1 `contains` q && p2 `contains` p
containedLeftOf _ _ _ = False


containedRightOf :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedRightOf p q (Is p1 (Direction _ (Just East)) p2) =
  p1 `contains` p && p2 `contains` q
containedRightOf p q (Is p1 (Direction _ (Just West)) p2) =
  p1 `contains` q && p2 `contains` p
containedRightOf _ _ _ = False


containedSameSpot :: AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool
containedSameSpot p q (Is p1 (Direction Nothing Nothing) p2) =
  p1 `contains` p && p2 `contains` q || p1 `contains` q && p2 `contains` p
containedSameSpot _ _ _ = False


{- |
True if the first argument is below the second and aligned on the X-axis.
-}
isSouthOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isSouthOf = compositeRelation containedSouthOf


{- |
True if the first argument is above the second and aligned on the X-axis.
-}
isNorthOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isNorthOf = compositeRelation containedNorthOf


{- |
True if the first argument is left of the second and aligned on the Y-axis.
-}
isWestOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isWestOf = compositeRelation containedWestOf


{- |
True if the first argument is right of the second and aligned on the Y-axis.
-}
isEastOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isEastOf = compositeRelation containedEastOf


{- |
True if the first argument is below and to the left of the second.
-}
isSouthWestOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isSouthWestOf = compositeRelation containedSouthWestOf


{- |
True if the first argument is below and to the right of the second.
-}
isSouthEastOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isSouthEastOf = compositeRelation containedSouthEastOf


{- |
True if the first argument is above and to the left of the second.
-}
isNorthWestOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isNorthWestOf = compositeRelation containedNorthWestOf


{- |
True if the first argument is above and to the right of the second.
-}
isNorthEastOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isNorthEastOf = compositeRelation containedNorthEastOf


{- |
True if the first argument is above the second, ignoring horizontal positioning.
-}
isAbove :: AbstractPicture -> AbstractPicture -> SpatialQuery
isAbove = compositeRelation containedAbove


{- |
True if the first argument is below the second, ignoring horizontal positioning.
-}
isBelow :: AbstractPicture -> AbstractPicture -> SpatialQuery
isBelow = compositeRelation containedBelow


{- |
True if the first argument is left of the second, ignoring vertical positioning.
-}
isLeftOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isLeftOf = compositeRelation containedLeftOf


{- |
True if the first argument is right of the second, ignoring vertical positioning.
-}
isRightOf :: AbstractPicture -> AbstractPicture -> SpatialQuery
isRightOf = compositeRelation containedRightOf


{- |
True if the first argument is at the same position as the second.
-}
atSamePosition :: AbstractPicture -> AbstractPicture -> SpatialQuery
atSamePosition = compositeRelation containedSameSpot


compositeRelation
  :: (AbstractPicture -> AbstractPicture -> RelativePicSpec -> Bool)
  -> AbstractPicture
  -> AbstractPicture
  -> SpatialQuery
compositeRelation g p q rs = all (`any` rs) allRelations
  where
    allRelations = [g x y | x <- getSubPictures p, y <- getSubPictures q]


toRelative :: AbstractPicture -> Components
toRelative p = case p of
  Pictures ps -> Components (Pictures ps, sort $ relativePosition ps)
  a           -> let noTranslation = stripTranslation a in
    Components (a,[alone noTranslation])


relativePosition :: [AbstractPicture] -> [RelativePicSpec]
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
  :: AbstractPicture
  -> AbstractPicture
  -> AbstractPicture
  -> RelativePicSpec
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
