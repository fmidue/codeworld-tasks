{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT

import Data.Text                        (Text)
import Data.Text.Lazy.Builder           (toLazyText)
import Haskell.Template.Task            (grade)
import Rainbow (
  Chunk,
  Radiant,
  back,
  fore,
  bold,
  chunk,
  brightBlue,
  brightCyan,
  brightMagenta,
  brightRed,
  brightYellow,
  green,
  red,
  yellow,
  putChunkLn,
  )
import System.Directory                 (getTemporaryDirectory)
import System.Exit                      (die, exitFailure, exitSuccess)
import System.Environment               (getArgs)
import System.FilePath                  ((-<.>))
import System.IO                        (readFile')
import Text.PrettyPrint.Leijen.Text     (Doc, SimpleDoc(..), renderPretty)



-- Configure output styling

statusLabel :: Radiant -> Chunk -> Chunk
statusLabel = (bold .) . back

errorMessageStyle :: Text -> Chunk
errorMessageStyle =  bold . fore brightMagenta . chunk

errorStyle :: Text -> Chunk
errorStyle = bold . fore brightRed . chunk

templateMarkerStyle :: Text -> Chunk
templateMarkerStyle = bold . fore brightCyan . chunk

suggestionStyle :: Text -> Chunk
suggestionStyle = bold . fore brightYellow . chunk


main :: IO ()
main = do
  args <- getArgs
  case args of
    [task,submission] -> do
        taskContents <- openDotHs task
        submissionContents <- openDotHs submission
        runTemplateTask taskContents submissionContents
        exitSuccess
    _                 -> die usage


runTemplateTask :: String -> String -> IO ()
runTemplateTask task submission = do
  tmp <- getTemporaryDirectory
  grade
    id
    rejection
    suggestion
    tmp
    task
    submission
  putChunkLn $ bold $ fore green "Your submission passed!"
  emptyLine
  putChunkLn $ statusLabel green "SUCCESS"


openDotHs :: FilePath -> IO String
openDotHs path = readFile' $ path -<.> "hs"


usage :: String
usage = "usage: test-task <config path> <solution path>"


styleRejections :: Text -> Chunk
styleRejections t
  | "Unless you fix the above" `T.isPrefixOf` t = errorMessageStyle t
  | "### Failure in:" `T.isPrefixOf` t = errorStyle t
  | "Your solution does not fit the template:" `T.isPrefixOf` t = errorStyle t
  | "^^^" `T.isInfixOf` t = templateMarkerStyle $ " " <> T.drop 1 t
  | "Solution" `T.isPrefixOf` t = errorStyle t
  | otherwise = styleCommon t


styleSuggestions :: Text -> Chunk
styleSuggestions t
  | "Solution" `T.isPrefixOf` t = suggestionStyle t
  | otherwise = styleCommon t


styleCommon :: Text -> Chunk
styleCommon t
  | t `elem` ["Template:", "Submission:", "Found:","Perhaps:", "Failing test case:"] = bold $ fore brightBlue $ chunk t
  | otherwise = bold $ chunk t


suggestion :: Doc -> IO ()
suggestion doc = do
  putChunkLn $ statusLabel yellow "SUGGESTION:"
  emptyLine
  unlinesChunks $ map styleSuggestions $ toLines doc
  emptyLine


rejection :: Doc -> IO a
rejection doc = do
  putChunkLn $ statusLabel red "ERROR:"
  emptyLine
  unlinesChunks $ map styleRejections $ toLines doc
  emptyLine
  putChunkLn $ statusLabel red "REJECTED"
  exitFailure


emptyLine :: IO ()
emptyLine = putStrLn ""


toLines :: Doc -> [Text]
toLines = concatMap T.lines . docToLines . renderPretty 0.4 80
  where
    docToLines SEmpty        = [""]
    docToLines (SChar _ _)   = []
    docToLines (SText _ s x) = LT.toStrict (toLazyText s) : docToLines x
    docToLines (SLine i x)   = "\n" <> spaces i : docToLines x

    spaces n
      | n <= 0 = ""
      | otherwise = LT.toStrict $ LT.replicate n (LT.singleton ' ')


unlinesChunks :: [Chunk] -> IO ()
unlinesChunks = mapM_ putChunkLn
