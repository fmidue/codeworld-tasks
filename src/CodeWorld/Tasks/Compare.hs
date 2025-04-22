{-# language RankNTypes #-}

module CodeWorld.Tasks.Compare (
  testCSE,
) where


import Data.Char                        (toLower)
import Data.List.Extra                  (maximumOn, minimumOn)
import Data.Maybe                       (fromJust)
import Data.Tuple.Extra                 (second, both)
import qualified Data.IntMap            as IM

import CodeWorld.Tasks.API              (Picture)
import CodeWorld.Tasks.HashCons         (BiMap, Node(..), hashconsShare)
import CodeWorld.Tasks.Reify            (ReifyPicture(..), share)



testCSE :: Picture -> IO (Maybe String)
testCSE a = do
  reifyResult <- share a
  let (explicitShares,termIndex) = both IM.toList reifyResult
  let (allShares,consTerms) = both toReify $ hashconsShare a
  let usedBinds = bindMapping explicitShares termIndex
  let possibleBinds = bindMapping allShares consTerms
  if length termIndex == length consTerms
    then
      pure Nothing
    else do
      let completeTerm = restoreTerm usedBinds termIndex $ minimumOn fst termIndex
      let explicit = map (restoreTerm usedBinds termIndex) explicitShares
      let sharable = map (restoreTerm possibleBinds consTerms) allShares
      let completeCons = restoreTerm possibleBinds consTerms $ maximumOn fst consTerms
      let feedback = unlines
            [ "There are opportunities for further sharing!"
            , "Consider your original term (with possibly renamed bindings):"
            , printSharedTerm completeTerm $ termsWithNames usedBinds explicitShares explicit
            , ""
            , "It could be rewritten in the following way:"
            , printSharedTerm completeCons $ termsWithNames possibleBinds allShares sharable
            ]
      pure $ Just feedback
  where
    restoreTerm bindings termLookup = printOriginal bindings termLookup . snd
    termsWithNames bindings shares = zip (map (fromJust . flip lookup bindings . fst) shares)

    printSharedTerm term shared = unlines $
      [ ""
      , "let"
      ] ++
      map (\(name,value) -> "  " ++ name ++ " = " ++ value) shared ++
      [  "in"
      ,  "  " ++ term
      ]


bindMapping :: [(Int,ReifyPicture Int)] -> [(Int,ReifyPicture Int)] -> [(Int,String)]
bindMapping = runMapping (1 :: Int)
  where
    runMapping _ _ [] = []
    runMapping step sharedTerms (x@(num,_):allTerms)
      | x `elem` sharedTerms = (num,"name" ++ show step) : runMapping (step+1) sharedTerms allTerms
      | otherwise = runMapping step sharedTerms allTerms


printOriginal :: [(Int,String)] -> [(Int, ReifyPicture Int)] -> ReifyPicture Int -> String
printOriginal bindings termLookup term = sub
  where
    printNext :: Int -> String
    printNext i = case lookup i bindings of
      Nothing
          | hasArguments reifyPic -> "(" ++ printOriginal bindings termLookup reifyPic ++ ")"
          | otherwise             -> printOriginal bindings termLookup reifyPic
        where reifyPic = fromJust $ lookup i termLookup
      Just name -> name

    printNextAnd :: Int -> String
    printNextAnd i = case lookup i bindings of
      Nothing -> printOriginal bindings termLookup $ fromJust $ lookup i termLookup
      Just name -> name

    sub = unwords $ case term of
      Color c i       -> ["colored", map toLower (show c), printNext i]
      Translate x y i -> ["translated", show x, show y, printNext i]
      Scale x y i     -> ["scaled", show x, show y, printNext i]
      Dilate fac i    -> ["dilated", show fac, printNext i]
      Rotate a i      -> ["rotated", show a, printNext i]
      Reflect a i     -> ["reflected", show a, printNext i]
      Clip x y i      -> ["clipped", show x, show y, printNext i]
      Pictures is     -> ["pictures", concatMap printNext is]
      And i1 i2       -> [printNextAnd i1, "&", printNextAnd i2]
      _               -> case show term of
        (x:xs) -> [toLower x:xs]
        _      -> error "not possible"


hasArguments :: ReifyPicture a -> Bool
hasArguments Blank           = False
hasArguments CoordinatePlane = False
hasArguments Logo            = False
hasArguments _               = True


toReify :: BiMap Node -> [(IM.Key, ReifyPicture Int)]
toReify = map $ second toReifyPic
  where
    toReifyPic n = case n of
      RectangleNode x y -> Rectangle x y
      ThickRectangleNode t x y -> ThickRectangle t x y
      SolidRectangleNode x y -> SolidRectangle x y
      CircleNode r -> Circle r
      ThickCircleNode t r -> ThickCircle t r
      SolidCircleNode r -> SolidCircle r
      PolygonNode ps -> Polygon ps
      SolidPolygonNode ps -> SolidPolygon ps
      ThickPolygonNode t ps -> ThickPolygon t ps
      ClosedCurveNode ps -> ClosedCurve ps
      SolidClosedCurveNode ps -> SolidClosedCurve ps
      ThickClosedCurveNode t ps -> ThickClosedCurve t ps
      PolylineNode ps -> Polyline ps
      ThickPolylineNode t ps -> ThickPolyline t ps
      CurveNode ps -> Curve ps
      ThickCurveNode t ps -> ThickCurve t ps
      SectorNode a1 a2 r -> Sector a1 a2 r
      ArcNode a1 a2 r -> Arc a1 a2 r
      ThickArcNode t a1 a2 r -> ThickArc t a1 a2 r
      LetteringNode t -> Lettering t
      StyledLetteringNode ts f t -> StyledLettering ts f t
      ColorNode c p -> Color c p
      TranslateNode x y p -> Translate x y p
      ScaleNode x y p -> Scale x y p
      DilateNode fac p -> Dilate fac p
      RotateNode a p -> Rotate a p
      ReflectNode a p -> Reflect a p
      ClipNode x y p -> Clip x y p
      PicturesNode ps -> Pictures ps
      AndNode p1 p2 -> And p1 p2
      CoordinatePlaneNode -> CoordinatePlane
      LogoNode -> Logo
      BlankNode -> Blank
