
module CodeWorld.Test.Abstract where


import Data.Tuple.Extra                 (both)
import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.Types (Color(..))

someRectangle :: Drawable a => a
someRectangle = rectangle 1 1

someSolidRectangle :: Drawable a => a
someSolidRectangle = solidRectangle 1 1

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
