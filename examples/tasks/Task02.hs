module Task02 where

import CodeWorld
import Prelude hiding (($)) -- just preventing some syntax that might confuse beginners

-- Draw a picture of a tree similar to: 
-- 
--   https://code.world/run.html?mode=haskell&dhash=DDZ_AqUvWQm3FricOIoNx7A 
--
-- Your tree should at least consist of a trunk, two branches and some 
-- leaves/crown. 
--
-- You can look up how to produce and transform relevant shapes in the
-- CodeWorld documentation and in the example(s) from the lecture. 

main :: IO ()
main = drawingOf scene

scene :: Picture
scene = undefined

