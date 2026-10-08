{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module DeriveLaws where


import Control.Concurrent
import Control.Monad (forever, void)
import Data.Text (Text)
import Graphics.Blank (blankCanvas)
import QuickSpec
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()

import CodeWorld
import CodeWorld.ImageExport (renderPictureDataURL)



sig :: Sig
sig = signature
  [ con "rectangle" rectangle
  , con "thickRectangle" thickRectangle
  , con "solidRectangle" solidRectangle
  , con "circle" circle
  , con "thickCircle" thickCircle
  , con "solidCircle" solidCircle
  , con "arc" arc
  , con "thickArc" thickArc
  , con "sector" sector
  , con "blank" blank
  , con "codeWorldLogo" codeWorldLogo
  , con "coordinatePlane" coordinatePlane
  , con "lettering" lettering
  , con "styledLettering" styledLettering
  , con "polyline" polyline
  , con "thickPolyline" thickPolyline
  , con "polygon" polygon
  , con "thickPolygon" thickPolygon
  , con "solidPolygon" solidPolygon
  , con "curve" curve
  , con "thickCurve" thickCurve
  , con "closedCurve" closedCurve
  , con "thickClosedCurve" thickClosedCurve
  , con "solidClosedCurve" solidClosedCurve
  , con "translated" translated
  , con "scaled" scaled
  , con "colored" colored
  , con "rotated" rotated
  , con "reflected" reflected
  , con "clipped" clipped
  , con "&" (&)
  , con "pictures" pictures
  ]

sigTypes :: Sig
sigTypes = signature
  [ monoObserve @Picture
  , mono @[Picture]
  , monoObserve @Color
  , mono @Text
  , mono @TextStyle
  , mono @Font
  , vars ["x","y"] $ Proxy @Double
  , vars ["s"] $ Proxy @TextStyle
  , vars ["f"] $ Proxy @Font
  , vars ["c"] $ Proxy @Color
  , vars ["p"] $ Proxy @Picture
  , vars ["ps"] $ Proxy @[Picture]
  , vars ["pts"] $ Proxy @[Point]
  , withMaxTestSize 10
  , withPrintStyle ForHumans
  , withPruningTermSize 9
  ]

sigBg :: Sig
sigBg = background
  [ arith $ Proxy @Double
  , con "*" $ (*) @Double
--  , con "-" $ (-) @Double
--  , con "negate" $ negate @Double
--  , con @Double "2*pi" (2*pi)
--  , con @Double "pi" pi
  , lists
  , con "translatedPoint" translatedPoint
  , con "scaledPoint" scaledPoint
  , con "reflectedPoint" reflectedPoint
  , con "rotatedPoint" rotatedPoint
  , con "foldr" $ foldr @[] @Picture @Picture
  , con "map" $ map @A @B
  ]


instance Observe
  (
  )
  Text
  Picture where
  observe _ = obs


{-# NOINLINE obs #-}
obs :: Picture -> Text
obs p = unsafePerformIO $ do
  canvas <- readMVar rendererVar
  renderPicture canvas p


data RenderRequest
  = Render Picture (MVar Text)


newtype Renderer = Renderer
  { rendererRequests :: Chan RenderRequest
  }


startRenderer :: Int -> IO Renderer
startRenderer port = do
  requests <- newChan
  ready <- newEmptyMVar
  void $ forkIO $ blankCanvas (fromIntegral port) $ \ctx -> do
    putMVar ready ()
    forever $ do
      Render p result <- readChan requests
      img <- renderPictureDataURL p ctx
      putMVar result img
  takeMVar ready
  pure Renderer {rendererRequests = requests}


renderPicture :: Renderer -> Picture -> IO Text
renderPicture renderer p = do
  result <- newEmptyMVar
  writeChan (rendererRequests renderer) $ Render p result
  takeMVar result


instance Observe () (Double,Double,Double,Double) Color where
  observe () c = (hue c, saturation c, luminosity c, alpha c)


instance Arbitrary Color where
  arbitrary = do
    n <- arbitrary `suchThat` (>=0)
    pure $ assortedColors !! n


instance Arbitrary TextStyle where
  arbitrary = elements
    [ Plain
    , Bold
    , Italic
    ]


instance Arbitrary Font where
  arbitrary = elements
    [ SansSerif
    , Serif
    , Monospace
    , Handwriting
    , Fancy
    , NamedFont "Font"
    ]


instance Arbitrary Picture where
  arbitrary = sized $ \n ->
    if n <= 1
      then basic
      else frequency
        [ (1, translated <$> arbitrary <*> arbitrary <*> decayArbitrary 2)
        , (1, scaled <$> arbitrary <*> arbitrary <*> decayArbitrary 2)
        , (1, colored <$> arbitrary <*> decayArbitrary 2)
        , (1, rotated <$> arbitrary <*> decayArbitrary 2)
        , (1, reflected <$> arbitrary <*> decayArbitrary 2)
        , (1, clipped <$> arbitrary <*> arbitrary <*> decayArbitrary 2)
        , (1, (&) <$> decayArbitrary 2 <*> decayArbitrary 2)
        , (2, pictures <$> pictureList)
        ]


basic :: Gen Picture
basic = frequency
  [ (1, pure blank)
  , (2, rectangle <$> nonNegative <*> nonNegative)
  , (2, thickRectangle <$> nonNegative <*> nonNegative <*> nonNegative)
  , (2, solidRectangle <$> nonNegative <*> nonNegative)
  , (2, circle <$> nonNegative)
  , (2, solidCircle <$> nonNegative)
  , (2, uncurry thickCircle <$> validThicknessRatio)
  , (2, arc <$> arbitrary <*> arbitrary <*> nonNegative)
  , (2, thickArc <$> nonNegative <*> arbitrary <*> arbitrary <*> nonNegative)
  , (2, sector <$> arbitrary <*> arbitrary <*> nonNegative)
  , (2, lettering <$> arbitrary)
  , (2, styledLettering <$> arbitrary <*> arbitrary <*> arbitrary)
  , (2, polyline <$> arbitrary)
  , (2, thickPolyline <$> nonNegative <*> arbitrary)
  , (2, polygon <$> arbitrary)
  , (2, thickPolygon <$> nonNegative <*> arbitrary)
  , (2, solidPolygon <$> arbitrary)
  , (2, curve <$> arbitrary)
  , (2, thickCurve <$> nonNegative <*> arbitrary)
  , (2, closedCurve <$> arbitrary)
  , (2, thickClosedCurve <$> nonNegative <*> arbitrary)
  , (2, solidClosedCurve <$> arbitrary)
  ]
  where
    nonNegative = getNonNegative <$> arbitrary

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary


pictureList :: Gen [Picture]
pictureList = do
  m <- getSize
  k  <- chooseInt (0, m)
  vectorOf k (resize (m-k) arbitrary)


validThicknessRatio :: Gen (Double, Double)
validThicknessRatio = do
  size <- getNonNegative <$> arbitrary
  thickness <- choose (0,size*2)
  pure (thickness, size)


{-# NOINLINE rendererVar #-}
rendererVar :: MVar Renderer
rendererVar = unsafePerformIO newEmptyMVar


main :: IO ()
main = do
  renderer <- startRenderer 3000
  putMVar rendererVar renderer
  quickSpec $ sig <> sigTypes <> sigBg
