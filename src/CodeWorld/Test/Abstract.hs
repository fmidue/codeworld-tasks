
module CodeWorld.Test.Abstract where


import Data.Tuple.Extra                 (both)
import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.Types (Color(..))

someSquare :: Drawable a => a
someSquare = rectangle 1 1

someSolidSquare :: Drawable a => a
someSolidSquare = solidRectangle 1 1

-- temporary synonym to not break existing tasks
someRectangle :: Drawable a => a
someRectangle = someSquare

-- temporary synonym to not break existing tasks
someSolidRectangle :: Drawable a => a
someSolidRectangle = someSolidSquare

someWideRectangle :: Drawable a => a
someWideRectangle = rectangle 2 1

someWideSolidRectangle :: Drawable a => a
someWideSolidRectangle = solidRectangle 2 1

someTallRectangle :: Drawable a => a
someTallRectangle = rectangle 1 2

someTallSolidRectangle :: Drawable a => a
someTallSolidRectangle = solidRectangle 1 2

someCircle :: Drawable a => a
someCircle = circle 1

someSolidCircle :: Drawable a => a
someSolidCircle = solidCircle 1

someColor :: Drawable a => a -> a
someColor = colored AnyColor

rotatedQuarter :: Drawable a => a -> a
rotatedQuarter = rotated $ pi/4

rotatedHalf :: Drawable a => a -> a
rotatedHalf = rotated $ 3*pi/4

rotatedThreeQuarters :: Drawable a => a -> a
rotatedThreeQuarters = rotated $ 5*pi/4

rotatedUpToFull :: Drawable a => a -> a
rotatedUpToFull = rotated $ 7*pi/4

smaller :: Drawable a => a -> a
smaller = dilated 0.5

larger :: Drawable a => a -> a
larger = dilated 2

smallerX :: Drawable a => a -> a
smallerX = scaled 0.5 1

largerX :: Drawable a => a -> a
largerX = scaled 2 1

smallerY :: Drawable a => a -> a
smallerY = scaled 1 0.5

largerY :: Drawable a => a -> a
largerY = scaled 1 2


someCurve :: Drawable a => Int -> a
someCurve points = curve $ take (points+1) $ iterate (both (+0.1)) (1,0)


someSolidCurve :: Drawable a => Int -> a
someSolidCurve points = solidClosedCurve $ take (points+1) $ iterate (both (+0.1)) (1,0)
