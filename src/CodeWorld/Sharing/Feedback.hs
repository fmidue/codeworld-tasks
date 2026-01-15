
{-# language TypeApplications #-}

module CodeWorld.Sharing.Feedback (
  testCSE
) where


import Data.Char                        (isNumber, toLower, toUpper)
import Data.List.Extra                  (intercalate, maximumOn, minimumOn, replace)
import Data.Maybe                       (fromJust)
import Data.Tuple.Extra                 (second, both)
import qualified Data.IntMap            as IM

import CodeWorld                        (Picture)
import CodeWorld.Sharing.HashCons       (BiMap, Node(..), hashconsShare)
import CodeWorld.Tasks.Picture          (share, toInterface)
import CodeWorld.Tasks.Types            (ReifyPicture(..), Shape(..), Style(..))



{- |
Produce student feedback on common subexpression elimination for a `Picture` value.
This compares the results of the Hashcons and Reify methods to determine unused sharing potential.
Returns `Nothing` if all possible terms are shared.

This runs in an `IO` context, since Reify uses `System.Mem.StableName.StableName` to track sharing.
-}
testCSE :: Picture -> IO (Maybe String)
testCSE p = do
  reifyResult <- share p
  let
    (explicitShares,termIndex) = both IM.toList reifyResult
    (allShares,consTerms) = both toReify $ hashconsShare $ toInterface p
  if length termIndex == length consTerms
    then
      pure Nothing
    else do
      let
        usedBinds = bindMapping explicitShares termIndex
        possibleBinds = bindMapping allShares consTerms
        completeTerm = restoreTerm usedBinds termIndex $ minimumOn fst termIndex
        explicit = map (restoreTerm usedBinds termIndex) explicitShares
        sharable = map (restoreTerm possibleBinds consTerms) allShares
        completeCons = restoreTerm possibleBinds consTerms $ maximumOn fst consTerms
      pure $ Just $ unlines
        [ "There are opportunities for common subexpression elimination (CSE) in your submission!"
        , "Consider this expression resembling your submission, possibly differing in the following ways:"
        , "  - Subexpressions distributed over multiple definitions have been combined into a single expression"
        , "  - Mathematical subexpressions have been fully evaluated"
        , "  - Some picture related subexpressions might also be fully or partially evaluated."
        , "  - Already defined bindings might have been renamed"
        , "  - Used 'where' bindings have been converted to 'let' bindings"
        , "  - Bindings which are not relevant to CSE have been removed"
        , ""
        , printSharedTerm completeTerm $ termsWithNames usedBinds explicitShares explicit
        , ""
        , ""
        , "It could be rewritten like this:"
        , ""
        , printSharedTerm completeCons $ termsWithNames possibleBinds allShares sharable
        , ""
        , ""
        , "The highlighted terms can be defined globally or locally at their use-site."
        , "For a local definition, you can use either a 'let' or 'where' binding."
        , "Of course, you can also change the proposed names to your liking, e.g. make them more concise."
        , "Also consider that your actual code is most likely " ++
          "structured slightly differently than this suggested improvement."
        , "As such, the location of the binding as shown here might also have to be adjusted."
        ]
  where
    restoreTerm bindings termLookup = printOriginal bindings termLookup . snd
    termsWithNames bindings shares = zip (map (fromJust . flip lookup bindings . fst) shares)

    printSharedTerm term shared
      | null shared = term
      | otherwise = unlines $
        "let" :
        map (\(name,value) -> "  " ++ name ++ " = " ++ value) shared ++
        [  "in"
        , addIndentLevel term
        ]


bindMapping :: [(Int,ReifyPicture Int)] -> [(Int,ReifyPicture Int)] -> [(Int,String)]
bindMapping sharedTerms allTerms = map toName (filter (`elem` sharedTerms) allTerms)
  where
    toName = second (formatBinding . ('a':) . capitalFirst . printOriginal [] allTerms)

    formatBinding = camelCase . filter keep . replace "&" "And"

    keep c = c `notElem` ['(',')',',','[',']','-','.','\n'] && not (isNumber c)

    capitalFirst [] = []
    capitalFirst (x:xs) = toUpper x : xs

    camelCase "" = ""
    camelCase " " = ""
    camelCase (' ':' ':s) = camelCase $ ' ' : s
    camelCase (' ':c:s) = toUpper c : camelCase s
    camelCase (c:s) = c : camelCase s

printOriginal :: [(Int,String)] -> [(Int, ReifyPicture Int)] -> ReifyPicture Int -> String
printOriginal bindings termLookup term = unwords $ case term of
  Color c i             ->
    [ "colored"
    , map toLower (parensShow c)
    , printNext i
    ]
  Translate x y i       ->
    [ "translated"
    , truncatedShow x
    , truncatedShow y
    , printNext i
    ]
  Scale x y i           ->
    [ "scaled"
    , truncatedShow x
    , truncatedShow y
    , printNext i
    ]
  Dilate fac i          ->
    [ "dilated"
    , truncatedShow fac
    , printNext i
    ]
  Rotate a i            ->
    [ "rotated"
    , truncatedShow a
    , printNext i
    ]
  Reflect a i           ->
    [ "reflected"
    , truncatedShow a
    , printNext i
    ]
  Clip x y i            ->
    [ "clipped"
    , truncatedShow x
    , truncatedShow y
    , printNext i
    ]
  Pictures is           ->
    indentedList "pictures" $ map printNextAnd is
  And i1 i2             ->
    [printNextAnd i1 ++ " &\n" ++ printNextAnd i2]
  Rectangle s x y       -> case s of
    Outline Nothing  -> "rectangle"
    Outline (Just t) -> unwords
      [ "thickRectangle"
      , truncatedShow t
      ]
    Solid            -> "solidRectangle"
    :
    [ truncatedShow x
    , truncatedShow y
    ]
  Circle s r            -> case s of
    Outline Nothing  -> "circle"
    Outline (Just t) -> unwords
      [ "thickCircle"
      , truncatedShow t
      ]
    Solid            -> "solidCircle"
    :
    [
    truncatedShow r
    ]
  Polyline s ps           -> case s of
    Open Nothing  -> indentedList "polyline" $ map show ps
    Open (Just t) -> indentedList ("thickPolyline " ++ truncatedShow t) $ map show ps
    Closed (Outline Nothing) -> indentedList "polygon" $ map show ps
    Closed (Outline (Just t)) -> indentedList ("thickPolygon " ++ truncatedShow t) $ map show ps
    Closed Solid -> indentedList "solidPolygon" $ map show ps
  Arc s a1 a2 r           -> case s of
    Outline Nothing -> "arc"
    Outline (Just t) -> "thickArc " ++ truncatedShow t
    Solid -> "sector"
    :
    [ truncatedShow a1
    , truncatedShow a2
    , truncatedShow r
    ]

  Curve s ps              -> case s of
    Open Nothing -> indentedList "curve" $ map show ps
    Open (Just t) -> indentedList ("thickCurve " ++ truncatedShow t) $ map show ps
    Closed (Outline Nothing) -> indentedList "closedCurve" $ map show ps
    Closed (Outline (Just t)) ->  indentedList ("thickClosedCurve " ++ truncatedShow t) $ map show ps
    Closed Solid -> indentedList "solidClosedCurve" $ map show ps

  Lettering t           ->
    ["lettering " ++ show t]
  StyledLettering s f t ->
    [ "styledLettering"
    , show s
    , show f
    , show t
    ]
  CoordinatePlane       ->
    ["coordinatePlane"]
  Logo                  ->
    ["codeWorldLogo"]
  Blank                 ->
    ["blank"]

  where
    printNext :: Int -> String
    printNext i = case lookup i bindings of
      Nothing
          | hasArguments reifyPic -> result
          | otherwise             -> originalTerm
        where reifyPic = fromJust $ lookup i termLookup
              originalTerm = printOriginal bindings termLookup reifyPic
              result = if '&' `elem` originalTerm || length originalTerm > maxLineWidth
                then "(\n" ++ addIndentLevel originalTerm ++ ")"
                else "(" ++ originalTerm ++ ")"
      Just name -> name

    printNextAnd :: Int -> String
    printNextAnd i = case lookup i bindings of
      Nothing -> printOriginal bindings termLookup $ fromJust $ lookup i termLookup
      Just name -> name

    roundTo :: Integer -> Double -> Double
    roundTo places d = (fromIntegral @Int . round $ d * fac) / fac
      where fac = 10^places

    parensShow :: Show a => a -> String
    parensShow = optParens . show

    truncatedShow = optParens . reSubPi . show . roundTo 3

    optParens s
      | length (words s) > 1  || any (`elem` s) "-/*" = '(': s ++ ")"
      | otherwise = s

    reSubPi "3.141" = "pi"
    reSubPi "1.570" = "pi/2"
    reSubPi "0.785" = "pi/4"
    reSubPi "4.712" = "3*pi/2"
    reSubPi "2.356" = "3*pi/4"
    reSubPi ('-':s) = '-': reSubPi s
    reSubPi s       = s


addIndentLevel :: String -> String
addIndentLevel = unlines . map ("  " ++) . lines


maxLineWidth :: Int
maxLineWidth = 80


indentedList :: String -> [String] -> [String]
indentedList name xs =
  [ name ++ "\n  ["
  , intercalate "\n  , " xs
  , "\n  ]"
  ]


hasArguments :: ReifyPicture a -> Bool
hasArguments Blank           = False
hasArguments CoordinatePlane = False
hasArguments Logo            = False
hasArguments _               = True


toReify :: BiMap Node -> [(IM.Key, ReifyPicture Int)]
toReify = map $ second toReifyPic
  where
    toReifyPic n = case n of
      RectangleNode x y -> Rectangle (Outline Nothing) x y
      ThickRectangleNode t x y -> Rectangle (Outline $ Just t) x y
      SolidRectangleNode x y -> Rectangle Solid x y
      CircleNode r -> Circle (Outline Nothing) r
      ThickCircleNode t r -> Circle (Outline $ Just t) r
      SolidCircleNode r -> Circle Solid r
      PolygonNode ps -> Polyline (Closed $ Outline Nothing) ps
      SolidPolygonNode ps -> Polyline (Closed Solid) ps
      ThickPolygonNode t ps -> Polyline (Closed $ Outline $ Just t) ps
      ClosedCurveNode ps -> Curve (Closed $ Outline Nothing) ps
      SolidClosedCurveNode ps -> Curve (Closed Solid) ps
      ThickClosedCurveNode t ps -> Curve (Closed $ Outline $ Just t) ps
      PolylineNode ps -> Polyline (Open Nothing) ps
      ThickPolylineNode t ps -> Polyline (Open $ Just t) ps
      CurveNode ps -> Curve (Open Nothing) ps
      ThickCurveNode t ps -> Curve (Open $ Just t) ps
      SectorNode a1 a2 r -> Arc Solid a1 a2 r
      ArcNode a1 a2 r -> Arc (Outline Nothing) a1 a2 r
      ThickArcNode t a1 a2 r -> Arc (Outline $ Just t) a1 a2 r
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
