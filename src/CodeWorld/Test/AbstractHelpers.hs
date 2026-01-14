
module CodeWorld.Test.AbstractHelpers where


import Data.Tuple.Extra                 (both)
import CodeWorld.Tasks.Picture
import CodeWorld.Tasks.Color            (Color(..))
import CodeWorld.Test.Abstract          (AbstractPicture, toConcretePicture)
import CodeWorld.Test.Rewrite           (normalizeAndAbstract)



{- |
Draw an abstract, hollow square.
-}
someSquare :: AbstractPicture
someSquare = normalizeAndAbstract $ rectangle 1 1

{- |
Draw an abstract, filled in square.
-}
someSolidSquare :: AbstractPicture
someSolidSquare = normalizeAndAbstract $ solidRectangle 1 1

{- |
Draw an abstract, hollow rectangle.
This is an alias for `someSquare`.
-}
someRectangle :: AbstractPicture
someRectangle = someSquare

{- |
Draw an abstract, filled in rectangle.
This is an alias for `someSolidSquare`.
-}
someSolidRectangle :: AbstractPicture
someSolidRectangle = someSolidSquare

{- |
Draw an abstract, hollow rectangle that is wider than tall.
-}
someWideRectangle :: AbstractPicture
someWideRectangle = normalizeAndAbstract $ rectangle 2 1

{- |
Draw an abstract, filled in rectangle that is wider than tall.
-}
someWideSolidRectangle :: AbstractPicture
someWideSolidRectangle = normalizeAndAbstract $ solidRectangle 2 1

{- |
Draw an abstract, hollow rectangle that is taller than wide.
-}
someTallRectangle :: AbstractPicture
someTallRectangle = normalizeAndAbstract $ rectangle 1 2

{- |
Draw an abstract, filled in rectangle that is taller than wide.
-}
someTallSolidRectangle :: AbstractPicture
someTallSolidRectangle = normalizeAndAbstract $ solidRectangle 1 2

{- |
Draw an abstract, hollow circle.
-}
someCircle :: AbstractPicture
someCircle = normalizeAndAbstract $ circle 1

{- |
Draw an abstract, filled in circle.
-}
someSolidCircle :: AbstractPicture
someSolidCircle = normalizeAndAbstract $ solidCircle 1

{- |
Provide an abstract shape with a color.
The color will be treated as equal with any other color.
-}
someColor :: AbstractPicture -> AbstractPicture
someColor = reNormalize $ colored AnyColor

{- |
Rotate an abstract shape by up to a quarter turn.
-}
rotatedQuarter :: AbstractPicture -> AbstractPicture
rotatedQuarter = reNormalize $ rotated $ pi/4

{- |
Rotate an abstract shape by between a quarter and half turn.
-}
rotatedHalf :: AbstractPicture -> AbstractPicture
rotatedHalf = reNormalize $ rotated $ 3*pi/4

{- |
Rotate an abstract shape by between a half and three quarters turn.
-}
rotatedThreeQuarters :: AbstractPicture -> AbstractPicture
rotatedThreeQuarters = reNormalize $ rotated $ 5*pi/4

{- |
Rotate an abstract shape by between a three quarters and up to a full turn.
The full turn itself is excluded.
-}
rotatedUpToFull :: AbstractPicture -> AbstractPicture
rotatedUpToFull = reNormalize $ rotated $ 7*pi/4

{- |
Shrink an abstract shape.
Both directions are scaled an equal amount.
-}
smaller :: AbstractPicture -> AbstractPicture
smaller = reNormalize $ dilated 0.5

{- |
Enlarge an abstract shape.
Both directions are scaled an equal amount.
-}
larger :: AbstractPicture -> AbstractPicture
larger = reNormalize $ dilated 2

{- |
Shrink an abstract shape in X-direction.
The Y-direction is unchanged.
-}
smallerX :: AbstractPicture -> AbstractPicture
smallerX = reNormalize $ scaled 0.5 1

{- |
Enlarge an abstract shape in X-direction.
The Y-direction is unchanged.
-}
largerX :: AbstractPicture -> AbstractPicture
largerX = reNormalize $ scaled 2 1

{- |
Shrink an abstract shape in Y-direction.
The X-direction is unchanged.
-}
smallerY :: AbstractPicture -> AbstractPicture
smallerY = reNormalize $ scaled 1 0.5

{- |
Enlarge an abstract shape in Y-direction.
The X-direction is unchanged.
-}
largerY :: AbstractPicture -> AbstractPicture
largerY = reNormalize $ scaled 1 2

{- |
Draw an abstract, open curve with this many segments.
-}
someCurve :: Int -> AbstractPicture
someCurve points = normalizeAndAbstract $
  curve $ take (points+1) $ iterate (both (+0.1)) (1,0)

{- |
Draw an abstract, filled in and closed curve with this many segments.
-}
someSolidCurve :: Int -> AbstractPicture
someSolidCurve points = normalizeAndAbstract $
  solidClosedCurve $ take (points+1) $ iterate (both (+0.1)) (1,0)

reNormalize :: (Picture -> Picture) -> (AbstractPicture -> AbstractPicture)
reNormalize f = normalizeAndAbstract . f . toConcretePicture
