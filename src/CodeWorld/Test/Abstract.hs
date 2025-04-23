
module CodeWorld.Test.Abstract where


import Data.Tuple.Extra                 (both)
import CodeWorld.Tasks.API              (Drawable(..))



someRectangle :: Drawable a => a
someRectangle = rectangle 1 1

someSolidRectangle :: Drawable a => a
someSolidRectangle = solidRectangle 1 1

someCircle :: Drawable a => a
someCircle = circle 1

someSolidCircle :: Drawable a => a
someSolidCircle = solidCircle 1

rotatedQuarter :: Drawable a => a -> a
rotatedQuarter = rotated (pi/2)

rotatedHalf :: Drawable a => a -> a
rotatedHalf = rotated pi

rotatedThreeQuarters :: Drawable a => a -> a
rotatedThreeQuarters = rotated (3*pi/2)

rotatedUpToFull :: Drawable a => a -> a
rotatedUpToFull = rotated 5

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
someCurve points = curve $ take points $ iterate (both (+0.1)) (1,0)


someSolidCurve :: Drawable a => Int -> a
someSolidCurve points = solidClosedCurve $ take points $ iterate (both (+0.1)) (1,0)
