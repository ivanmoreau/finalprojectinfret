{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Preprocess
import Text.LaTeX
import Data.Serialize
import IndexRep (Documents, _docsN, _wordDoc, _word, _words, _docs, _weights)
import GHC.Generics (Generic)
import Glue (readDocuments, parseFile, stage0doc, stage2doc, stage1doc, weightAll, getEveryScorePair, cosineSimilarity, genVector)
import qualified Data.ByteString as B
import Optics
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.LaTeX.Base.Syntax (LaTeX(TeXRaw))
import qualified Data.Map as M
import Text.LaTeX.Base.Pretty (prettyLaTeX)
import Text.LaTeX.Packages.LongTable (longtable)
import GHC.IO (unsafePerformIO)
import qualified Data.List as L
import System.Console.CmdArgs


handleParsing :: FilePath -> FilePath -> IO ()
handleParsing input output = do
  contents <- readDocuments input
  let pred = parseFile contents
  let info = fst $ unzip pred
  let pars = stage2doc $ stage1doc pred $ stage0doc pred
  let resu = (info, pars)
  B.writeFile output (encode resu)
  putStrLn "Done."
  return ()

listAsLine :: [T.Text] -> LaTeX
listAsLine t = let
  l = map TeXRaw t
  in foldl1 (Text.LaTeX.&) l <> TeXRaw "\\\\\n"

docGetWords :: (Num a, Show a) => Lens Documents Documents 
  (M.Map Int [(Int, a)]) (M.Map Int [(Int, a)]) -> Int -> Documents -> [T.Text]
docGetWords l i docs = let
  ww = flip (_wordDoc i) l
  r = flip map [0..(docs ^. _docsN)-1] $ \j -> docs ^. ww j Optics.& T.take 4 . T.pack . show . snd
  in r

-- All list in the list are the same length
allSameLength :: [[a]] -> Bool
allSameLength [] = True
allSameLength (x:xs) = all (\y -> length x == length y) xs


doc0La :: (Num a, Show a) => Documents -> Lens Documents Documents
  (M.Map Int [(Int, a)]) (M.Map Int [(Int, a)])
  -> LaTeX 
doc0La doc lens = let
  ndocs = doc ^. _docsN
  nwords = length $ doc ^. _words
  emptysep = Separator mempty 
  specs = L.intersperse (Separator $ TeXRaw "\\hspace{.04cm}") $ 
    LeftColumn : emptysep : VerticalLine : (replicate ndocs CenterColumn ++ [emptysep])
  postd = flip map [0..nwords - 1] $ \i -> ((doc ^. _words) !! i) : docGetWords lens i doc
  conv = map listAsLine postd :: [LaTeX]
  header = listAsLine $ "W/D" : map (T.pack . show) [0..ndocs-1] :: LaTeX
  inside = mconcat $ header : hline : conv
  in if allSameLength postd then longtable Nothing specs inside else error "Not all lists are the same length"

genMTable :: (Num a, Show a) => FilePath -> Lens Documents Documents
  (M.Map Int [(Int, a)]) (M.Map Int [(Int, a)])  
  -> FilePath
  -> IO ()
genMTable input lens out = do
  contents <- B.readFile input
  let (info, doc) = (decode contents ^? _Right Optics.& fromMaybe (error "Decode failed")) :: ([DocumentInfo], Documents)
  renderFile out $ doc0La doc lens
  return ()

genFirstTable :: FilePath -> IO ()
genFirstTable input = do
  _ <- genMTable input _docs "table0.tex"
  return ()

getSecondTable :: FilePath -> IO ()
getSecondTable input = do
  _ <- genMTable input _weights "table1.tex"
  return ()

docCalcRT :: Documents -> LaTeX
docCalcRT doc = let
  ndocs = doc ^. _docsN
  emptysep = Separator mempty
  specs = L.intersperse (Separator $ TeXRaw "\\hspace{.04cm}") $
    LeftColumn : emptysep : VerticalLine : (replicate ndocs CenterColumn ++ [emptysep])
  header = listAsLine $ "D/D" : map (T.pack . show) [0..ndocs-1] :: LaTeX
  fi a = listAsLine $ (T.pack $ show a) : (flip map [0..ndocs-1] $ 
    \i -> (T.pack . show) $ cosineSimilarity
    (genVector a doc) (genVector i doc))
  inside = mconcat $ header : hline : map fi [0..ndocs-1]
  in tabular Nothing specs inside

genDocCalcTable :: FilePath -> IO ()
genDocCalcTable input = do
  contents <- B.readFile input
  let (info, doc) = (decode contents ^? _Right Optics.& fromMaybe (error "Decode failed")) :: ([DocumentInfo], Documents)
  renderFile "table2.tex" $ docCalcRT doc
  return ()

handleLaTeX :: FilePath -> IO ()
handleLaTeX input = do
  genFirstTable input
  getSecondTable input
  genDocCalcTable input

handleDocs :: FilePath -> IO ()
handleDocs input = do
  contents <- B.readFile input
  let (info, doc) = (decode contents ^? _Right Optics.& fromMaybe (error "Decode failed")) :: ([DocumentInfo], Documents)
  let words = doc ^. _docs
  putStr $ show words
  return ()

handleWeights :: FilePath -> IO ()
handleWeights input = do
  contents <- B.readFile input
  let (info, doc) = (decode contents ^? _Right Optics.& fromMaybe (error "Decode failed")) :: ([DocumentInfo], Documents)
  let weights = doc ^. _weights
  putStr $ show weights
  return ()

handleWords :: FilePath -> IO ()
handleWords input = do
  contents <- B.readFile input
  let (info, doc) = (decode contents ^? _Right Optics.& fromMaybe (error "Decode failed")) :: ([DocumentInfo], Documents)
  let words = doc ^. _words
  putStr $ show $ zip [0..] words
  return ()

handleSimititudes :: FilePath -> Bool -> IO ()
handleSimititudes input order = do
  contents <- B.readFile input
  let (info, doc) = (decode contents ^? _Right Optics.& fromMaybe (error "Decode failed")) :: ([DocumentInfo], Documents)
  let simititudes = getEveryScorePair doc
  putStr $ show $ if order then L.sortOn snd simititudes else simititudes
  return ()

data SimilitudCoseno =
  Parser { input_ :: FilePath , output :: FilePath }
  | LaTeX { input_ :: FilePath }
  | Docs { input_ :: FilePath }
  | Weights { input_ :: FilePath }
  | Words { input_ :: FilePath }
  | Similitudes { input_ :: FilePath , inOrder :: Bool }
  deriving (Show, Data, Typeable)

doParser = Parser { input_ = def , output = def }
doLaTeX = LaTeX { input_ = def }
doDocs = Docs { input_ = def }
doWeights = Weights { input_ = def }
doWords = Words { input_ = def }
doSimilitudes = Similitudes { input_ = def , inOrder = False }

something :: SimilitudCoseno -> IO ()
something (Parser input output) = handleParsing input output
something (LaTeX input) = handleLaTeX input
something (Docs input) = handleDocs input
something (Weights input) = handleWeights input
something (Words input) = handleWords input
something (Similitudes input order) = handleSimititudes input order

main :: IO ()
main = do
  args <- cmdArgs (modes [doParser, doLaTeX, doDocs, doWeights, doSimilitudes, doWords])
  something args
  return ()
