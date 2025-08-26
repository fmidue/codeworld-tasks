module Task01 where

import CodeWorld
import Prelude hiding (($)) -- just preventing some syntax that might confuse beginners

-- Draw a yellow solid circle and a green solid rectangle below it -
-- not behind it!
--
-- As guidance, the result should look roughly like this:
-- https://code.world/run.html?mode=haskell&dhash=DEpssmhzFT22fqxwW2yXJVQ
--
-- Consider the example(s) from the lecture and also look up how to
-- produce and transform relevant shapes in the CodeWorld
-- documentation.

scene :: Picture
scene = translated 0 6 (colored yellow (solidCircle 1)) & colored green (solidRectangle 20 2)

main :: IO ()
main = drawingOf scene

