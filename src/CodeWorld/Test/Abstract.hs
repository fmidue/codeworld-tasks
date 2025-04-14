
module CodeWorld.Test.Abstract where


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
