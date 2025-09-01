
module CodeWorld.Test.Abstract where


import Data.Tuple.Extra                 (both)
import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.Color            (Color(..))



{- |
Draw an abstract, hollow square.
-}
someSquare :: Drawable a => a
someSquare = rectangle 1 1

{- |
Draw an abstract, filled in square.
-}
someSolidSquare :: Drawable a => a
someSolidSquare = solidRectangle 1 1

{- |
Draw an abstract, hollow rectangle.
This is the same function as `someSquare`.
-}
someRectangle :: Drawable a => a
someRectangle = someSquare

{- |
Draw an abstract, filled in rectangle.
This is the same function as `someSolidSquare`.
-}
someSolidRectangle :: Drawable a => a
someSolidRectangle = someSolidSquare

{- |
Draw an abstract, hollow rectangle that is wider than tall.
-}
someWideRectangle :: Drawable a => a
someWideRectangle = rectangle 2 1

{- |
Draw an abstract, filled in rectangle that is wider than tall.
-}
someWideSolidRectangle :: Drawable a => a
someWideSolidRectangle = solidRectangle 2 1

{- |
Draw an abstract, hollow rectangle that is taller than wide.
-}
someTallRectangle :: Drawable a => a
someTallRectangle = rectangle 1 2

{- |
Draw an abstract, filled in rectangle that is taller than wide.
-}
someTallSolidRectangle :: Drawable a => a
someTallSolidRectangle = solidRectangle 1 2

{- |
Draw an abstract, hollow circle.
-}
someCircle :: Drawable a => a
someCircle = circle 1

{- |
Draw an abstract, filled in circle.
-}
someSolidCircle :: Drawable a => a
someSolidCircle = solidCircle 1

{- |
Provide an abstract shape with a color.
The color will be treated as equal with any other color.
-}
someColor :: Drawable a => a -> a
someColor = colored AnyColor

{- |
Rotate an abstract shape by up to a quarter turn.
-}
rotatedQuarter :: Drawable a => a -> a
rotatedQuarter = rotated $ pi/4

{- |
Rotate an abstract shape by between a quarter and half turn.
-}
rotatedHalf :: Drawable a => a -> a
rotatedHalf = rotated $ 3*pi/4

{- |
Rotate an abstract shape by between a half and three quarters turn.
-}
rotatedThreeQuarters :: Drawable a => a -> a
rotatedThreeQuarters = rotated $ 5*pi/4

{- |
Rotate an abstract shape by between a three quarters and up to a full turn.
The full turn itself is excluded.
-}
rotatedUpToFull :: Drawable a => a -> a
rotatedUpToFull = rotated $ 7*pi/4

{- |
Shrink an abstract shape.
Both directions are scaled an equal amount.
-}
smaller :: Drawable a => a -> a
smaller = dilated 0.5

{- |
Enlarge an abstract shape.
Both directions are scaled an equal amount.
-}
larger :: Drawable a => a -> a
larger = dilated 2

{- |
Shrink an abstract shape in X-direction.
The Y-direction is unchanged.
-}
smallerX :: Drawable a => a -> a
smallerX = scaled 0.5 1

{- |
Enlarge an abstract shape in X-direction.
The Y-direction is unchanged.
-}
largerX :: Drawable a => a -> a
largerX = scaled 2 1

{- |
Shrink an abstract shape in Y-direction.
The X-direction is unchanged.
-}
smallerY :: Drawable a => a -> a
smallerY = scaled 1 0.5

{- |
Enlarge an abstract shape in Y-direction.
The X-direction is unchanged.
-}
largerY :: Drawable a => a -> a
largerY = scaled 1 2

{- |
Draw an abstract, open curve with this many segments.
-}
someCurve :: Drawable a => Int -> a
someCurve points = curve $ take (points+1) $ iterate (both (+0.1)) (1,0)

{- |
Draw an abstract, filled in and closed curve with this many segments.
-}
someSolidCurve :: Drawable a => Int -> a
someSolidCurve points = solidClosedCurve $ take (points+1) $ iterate (both (+0.1)) (1,0)
