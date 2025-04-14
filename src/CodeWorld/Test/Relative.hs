{-# language RecordWildCards #-}

module CodeWorld.Test.Relative (
  Components(..),
  RelativePicSpec(..),
  (===),
  northOf,
  southOf,
  eastOf,
  westOf,
  southeastOf,
  southwestOf,
  northeastOf,
  northwestOf,
  onTopOf,
  alone,
  toRelative,
  )where


import Data.List                        (sort)

import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Test.Normalize (
  NormalizedPicture(..),
  Moved(..),
  getExactPos,
  stripTranslation,
  getTranslation,
  couldHaveTranslation,
  )



data DirectionV = South deriving (Eq,Ord,Show)
data DirectionH = West | East deriving (Eq,Ord,Show)

data Direction = Direction {
  vertical :: Maybe DirectionV,
  horizontal :: Maybe DirectionH
} deriving (Eq,Ord)


data RelativePicSpec
  = Is NormalizedPicture Direction NormalizedPicture
  | Alone NormalizedPicture
  deriving(Eq,Ord)


newtype Components = Components ([NormalizedPicture],[RelativePicSpec]) deriving (Eq,Show)


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


toRelative :: NormalizedPicture -> Components
toRelative p = case p of
  Pictures ps -> Components (map stripTranslation ps, sort $ relativePosition ps)
  a           -> let noTranslation = stripTranslation a in
    Components ([noTranslation],[alone noTranslation])


relativePosition :: [NormalizedPicture] -> [RelativePicSpec]
relativePosition [] = []
relativePosition (p:ps)
  | couldHaveTranslation p = othersTrans ++ relativePosition ps
  | otherwise = others ++ relativePosition ps
  where
    (pX,pY) = getTranslation p

    asCenter pic = let (bX,bY) = getTranslation pic in
      translated (getExactPos $ bX-pX) (getExactPos $ bY-pY) $ stripTranslation pic

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
orientation (Translate a b _) = case (a,b) of
      (Zero , Neg _)   -> northOf
      (Zero , Pos _)   -> southOf
      (Neg _, Zero)   -> eastOf
      (Pos _, Zero)   -> westOf
      (Pos _, Pos _) -> southwestOf
      (Pos _, Neg _)  -> northwestOf
      (Neg _, Pos _)  -> southeastOf
      (Neg _, Neg _)  -> northeastOf
      _               -> error $
                           "This should never happen. " ++
                           "translated smart constructor wasn't used."
orientation _ = onTopOf
