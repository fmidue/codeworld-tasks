{-# language RankNTypes #-}

module CodeWorld.Tasks.Compare (
  runShare,
) where


import Data.Char                        (toLower)
import Data.Foldable                    (toList)
import Data.List.Extra                  ((\\), nubOrd)
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Tuple.Extra                 (second, both)
import qualified Data.IntMap            as IM

import CodeWorld.Tasks.API              (Drawable)
import CodeWorld.Tasks.HashCons         (BiMap, Node(..), hashconsShare)
import CodeWorld.Tasks.Reify            (ReifyPicture(..), share)



runShare :: (forall a . Drawable a => a) -> IO ([(IM.Key, ReifyPicture Int)], [(IM.Key, ReifyPicture Int)])
runShare a = do
  reifyResult <- share a
  let (explicitShares,termIndex) = both IM.toList reifyResult
  let allShares = relabelWith termIndex (hashconsShare a)
  let varM = varMapping explicitShares termIndex
  print varM
  if explicitShares == allShares
    then
      putStrLn "You shared everything, good job!"
    else do
      putStrLn "There are opportunities for further sharing!"
      putStrLn "Consider your original term (with possibly renamed bindings):"
      let completeTerm = case restoreTerms varM termIndex (filter (\x -> fst x == 1) termIndex) of
            []    -> error "this graph has no root"
            (x:_) -> x
      let explicit = restoreTerms varM termIndex explicitShares
      printSharedTerm completeTerm explicit
      putStrLn ""
      putStrLn "It could be rewritten in the following way:"
      let notShared = restoreTerms varM termIndex $ allShares \\ explicitShares

      mapM_ (printSuggestions completeTerm explicit) notShared
  pure (explicitShares,allShares)
  where
    restoreTerms bindings source = map (\x -> printOriginal bindings (snd x) source)

    printSharedTerm term shared = do
      putStrLn ""
      putStrLn "let"
      mapM_ (\(name,value) -> putStrLn $ "  " ++ name ++ " = " ++ value) sharingNames
      putStrLn "in"
      putStrLn $ "  " ++ term
      where
        sharingNames = consistentName 0 shared

    printSuggestions term alreadyShared subterm = do
      putStrLn ""
      putStrLn "let"
      mapM_ (\(name,value) -> putStrLn $ "  " ++ name ++ " = " ++ value) sharingNames
      putStrLn "in"
      putStrLn $ "  " ++ term
      where
        alreadyNamed = consistentName 0 alreadyShared
        sharingNames = alreadyNamed ++ consistentName (length alreadyNamed) [subterm]


varMapping :: [(Int,ReifyPicture Int)] -> [(Int,ReifyPicture Int)] -> [(Int,String)]
varMapping = runMapping (1 :: Int)
  where
    runMapping _ _ [] = []
    runMapping step sharedTerms (x@(num,_):allTerms)
      | x `elem` sharedTerms = (num,"name" ++ show step) : runMapping (step+1) sharedTerms allTerms
      | otherwise = runMapping step sharedTerms allTerms


consistentName :: Int -> [String] -> [(String, String)]
consistentName start = zip (drop start bindingSupply)
  where bindingSupply = map (("name" ++) . show) [1 :: Int ..]


printOriginal :: [(Int,String)] -> ReifyPicture Int -> [(Int, ReifyPicture Int)] -> String
printOriginal bindings term subTerms = sub term
  where
    getExpr i = case lookup i bindings of
                  Nothing   -> Left $ fromJust $ lookup i subTerms
                  Just name -> Right name

    printNext :: Int -> String
    printNext i = case getExpr i of
      Left reifyPic
        | hasArguments reifyPic -> "(" ++ printOriginal bindings reifyPic subTerms ++ ")"
        | otherwise             -> printOriginal bindings reifyPic subTerms
      Right name -> name

    printNextAnd :: Int -> String
    printNextAnd i = case getExpr i of
      Left reifyPic -> printOriginal bindings reifyPic subTerms
      Right name    -> name

    sub p = unwords $ case term of
      Color c i       -> ["colored", map toLower (show c), printNext i]
      Translate x y i -> ["translated", show x, show y, printNext i]
      Scale x y i     -> ["scaled", show x, show y, printNext i]
      Dilate fac i    -> ["dilated", show fac, printNext i]
      Rotate a i      -> ["rotated", show a, printNext i]
      Reflect a i     -> ["reflected", show a, printNext i]
      Clip x y i      -> ["clipped", show x, show y, printNext i]
      Pictures is     -> ["pictures", concatMap printNext is]
      And i1 i2       -> [printNextAnd i1, "&", printNextAnd i2]
      _               -> case show p of
        (x:xs) -> [toLower x:xs]
        _      -> error "not possible"


hasArguments :: ReifyPicture a -> Bool
hasArguments Blank           = False
hasArguments CoordinatePlane = False
hasArguments Logo            = False
hasArguments _               = True


relabelWith
  :: [(Int,ReifyPicture Int)]
  -> (BiMap Node,BiMap Node)
  -> [(Int,ReifyPicture Int)]
relabelWith reifyTerm (consShares,consTerm) = map updateNode $ toReify consShares
  where
    consRoot = maximum $ map fst consTerm
    reifyRoot = minimum $ map fst reifyTerm
    nodesAsReify = toReify consTerm
    renamingOrderReify = reifyRoot : graphNodes reifyTerm
    renamingOrderNodes = consRoot : reverse (graphNodes nodesAsReify)
    mapping = zip renamingOrderNodes renamingOrderReify
    updateNode (i,graph) = (updateIndex i,fmap updateIndex graph)
    updateIndex i = fromMaybe i (lookup i mapping)
    graphNodes = nubOrd . concatMap (toList . snd)


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
