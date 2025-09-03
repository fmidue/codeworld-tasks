
module CodeWorld.Test.Abstract where


import Data.Tuple.Extra                 (both)
import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.Color            (Color(..))
import CodeWorld.Test.Normalize         (NormalizedPicture)



{- |
Draw an abstract, hollow square.
-}
someSquare :: NormalizedPicture
someSquare = rectangle 1 1

{- |
Draw an abstract, filled in square.
-}
someSolidSquare :: NormalizedPicture
someSolidSquare = solidRectangle 1 1

{- |
Draw an abstract, hollow rectangle.
This is an alias for `someSquare`.
-}
someRectangle :: NormalizedPicture
someRectangle = someSquare

{- |
Draw an abstract, filled in rectangle.
This is an alias for `someSolidSquare`.
-}
someSolidRectangle :: NormalizedPicture
someSolidRectangle = someSolidSquare

{- |
Draw an abstract, hollow rectangle that is wider than tall.
-}
someWideRectangle :: NormalizedPicture
someWideRectangle = rectangle 2 1

{- |
Draw an abstract, filled in rectangle that is wider than tall.
-}
someWideSolidRectangle :: NormalizedPicture
someWideSolidRectangle = solidRectangle 2 1

{- |
Draw an abstract, hollow rectangle that is taller than wide.
-}
someTallRectangle :: NormalizedPicture
someTallRectangle = rectangle 1 2

{- |
Draw an abstract, filled in rectangle that is taller than wide.
-}
someTallSolidRectangle :: NormalizedPicture
someTallSolidRectangle = solidRectangle 1 2

{- |
Draw an abstract, hollow circle.
-}
someCircle :: NormalizedPicture
someCircle = circle 1

{- |
Draw an abstract, filled in circle.
-}
someSolidCircle :: NormalizedPicture
someSolidCircle = solidCircle 1

{- |
Provide an abstract shape with a color.
The color will be treated as equal with any other color.
-}
someColor :: NormalizedPicture -> NormalizedPicture
someColor = colored AnyColor

{- |
Rotate an abstract shape by up to a quarter turn.
-}
rotatedQuarter :: NormalizedPicture -> NormalizedPicture
rotatedQuarter = rotated $ pi/4

{- |
Rotate an abstract shape by between a quarter and half turn.
-}
rotatedHalf :: NormalizedPicture -> NormalizedPicture
rotatedHalf = rotated $ 3*pi/4

{- |
Rotate an abstract shape by between a half and three quarters turn.
-}
rotatedThreeQuarters :: NormalizedPicture -> NormalizedPicture
rotatedThreeQuarters = rotated $ 5*pi/4

{- |
Rotate an abstract shape by between a three quarters and up to a full turn.
The full turn itself is excluded.
-}
rotatedUpToFull :: NormalizedPicture -> NormalizedPicture
rotatedUpToFull = rotated $ 7*pi/4

{- |
Shrink an abstract shape.
Both directions are scaled an equal amount.
-}
smaller :: NormalizedPicture -> NormalizedPicture
smaller = dilated 0.5

{- |
Enlarge an abstract shape.
Both directions are scaled an equal amount.
-}
larger :: NormalizedPicture -> NormalizedPicture
larger = dilated 2

{- |
Shrink an abstract shape in X-direction.
The Y-direction is unchanged.
-}
smallerX :: NormalizedPicture -> NormalizedPicture
smallerX = scaled 0.5 1

{- |
Enlarge an abstract shape in X-direction.
The Y-direction is unchanged.
-}
largerX :: NormalizedPicture -> NormalizedPicture
largerX = scaled 2 1

{- |
Shrink an abstract shape in Y-direction.
The X-direction is unchanged.
-}
smallerY :: NormalizedPicture -> NormalizedPicture
smallerY = scaled 1 0.5

{- |
Enlarge an abstract shape in Y-direction.
The X-direction is unchanged.
-}
largerY :: NormalizedPicture -> NormalizedPicture
largerY = scaled 1 2

{- |
Draw an abstract, open curve with this many segments.
-}
someCurve :: Int -> NormalizedPicture
someCurve points = curve $ take (points+1) $ iterate (both (+0.1)) (1,0)

{- |
Draw an abstract, filled in and closed curve with this many segments.
-}
someSolidCurve :: Int -> NormalizedPicture
someSolidCurve points = solidClosedCurve $ take (points+1) $ iterate (both (+0.1)) (1,0)
