{-# language RankNTypes #-}

module CodeWorld.Tasks.Compare (
  runShare,
) where


import Data.Char                        (toLower)
import Data.Foldable                    (toList)
import Data.List.Extra                  ((\\), maximumOn, nubOrd, replace)
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Tuple.Extra                 (second, both)
import qualified Data.IntMap            as IM

import CodeWorld.Tasks.API              (Drawable)
import CodeWorld.Tasks.HashCons         (BiMap, Node(..), hashconsShare)
import CodeWorld.Tasks.Reify            (ReifyPicture(..), share)



runShare :: (forall a . Drawable a => a) -> IO ([(IM.Key, ReifyPicture Int)], [(IM.Key, ReifyPicture Int)])
runShare a = do
  (reify,reifyTerms) <- share a
  let explicitShares = IM.toList reify
  let (allShares,termIndex) = relabelWith (IM.toList reifyTerms) (hashconsShare a)
  if explicitShares == allShares
    then
      putStrLn "You shared everything, good job!"
    else do
      putStrLn "There are opportunities for further sharing!"
      putStrLn "Consider your original term (with possibly renamed bindings):"
      let completeTerm = maximumOn length $ map (flip printOriginal termIndex . snd) termIndex
      let explicit = map (flip printOriginal termIndex . snd) explicitShares
      printSharedTerm completeTerm explicit
      putStrLn ""
      putStrLn "It could be rewritten in the following way:"
      let notShared = map (flip printOriginal termIndex . snd) $ allShares \\ explicitShares

      mapM_ (printSuggestions completeTerm explicit) notShared
  pure (explicitShares,allShares)
  where
    rep = foldr (\(n,v) str -> substitute v n str)

    printSharedTerm term shared = do
      putStrLn ""
      putStrLn "let"
      mapM_ (\(name,value) -> putStrLn $ "  " ++ name ++ " = " ++ value) sharingNames
      putStrLn "in"

      putStrLn $ "  " ++ rep term sharingNames
      where
        sharingNames = consistentName 0 shared

    printSuggestions term alreadyShared subterm = do
      putStrLn ""
      putStrLn "let"
      mapM_ (\(name,value) -> putStrLn $ "  " ++ name ++ " = " ++ value) sharingNames
      putStrLn "in"
      putStrLn $ "  " ++ maintermWithSub
      where
        alreadyNamed = consistentName 0 alreadyShared
        sharingNames = alreadyNamed ++ consistentName (length alreadyNamed) [subtermWithSub]
        subtermWithSub = rep subterm alreadyNamed
        maintermWithSub = rep term $ reverse sharingNames


consistentName :: Int -> [String] -> [(String, String)]
consistentName start = zip (drop start bindingSupply)
  where bindingSupply = map (("name" ++) . show) [1 :: Int ..]


substitute :: String -> String -> String -> String
substitute subterm name term = replace subterm name $ replace withBrackets name term
  where
    withBrackets = '(' : subterm ++ ")"


printOriginal :: (Show a, Eq a) => ReifyPicture a -> [(a, ReifyPicture a)] -> String
printOriginal term subTerms = sub term
  where
    getExpr = fromJust . flip lookup subTerms
    recursively n
      | hasArguments expr = '(': printOriginal expr subTerms ++ ")"
      | otherwise = printOriginal expr subTerms
      where
        expr = getExpr n
    recursivelyAnd n = printOriginal (getExpr n) subTerms


    sub p = unwords $ case term of
      Color c i       -> ["colored", map toLower (show c), recursively i]
      Translate x y i -> ["translated", show x, show y, recursively i]
      Scale x y i     -> ["scaled", show x, show y, recursively i]
      Dilate fac i    -> ["dilated", show fac, recursively i]
      Rotate a i      -> ["rotated", show a, recursively i]
      Reflect a i     -> ["reflected", show a, recursively i]
      Clip x y i      -> ["clipped", show x, show y, recursively i]
      Pictures is     -> ["pictures", concatMap recursively is]
      And i1 i2       -> [recursivelyAnd i1, "&", recursivelyAnd i2]
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
  -> ([(Int,ReifyPicture Int)],[(Int,ReifyPicture Int)])
relabelWith reifyTerm (consShares,consTerm) =
    both (map updateNode) (toReify consShares,nodesAsReify)
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
