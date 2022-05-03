module Glue where

import qualified Data.Text as T
import qualified Data.List as L
import Optics
import IndexRep
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import Preprocess (parseDocument, DocumentInfo)
import Data.Traversable (for, forM)
import NLP.Stemmer
import Control.Parallel.Strategies (parMap, rdeepseq)

readDocuments :: FilePath -> IO T.Text
readDocuments path = do               
  contents <- readFile path
  return $ T.pack contents

testFile :: T.Text
testFile = unsafePerformIO $ readDocuments "ProyectoF.txt"

parseFile :: T.Text -> [(DocumentInfo , T.Text)]
parseFile = parseDocument

stage0doc :: [(DocumentInfo , T.Text)] -> Documents
stage0doc = prepareDoc . snd . unzip

stage1doc :: [(DocumentInfo , T.Text)] -> Documents -> Documents
stage1doc l doc = let
  justdocs = snd $ unzip l
  in doDocs justdocs doc

stage2doc :: Documents -> Documents
stage2doc = weightAll

addWeight :: Int -> Int -> Documents -> Documents
addWeight i doc docs = let
   calc = tfidf i doc docs
   in if calc > 0 then 
     docs & _wordDoc i doc _weights .~ (doc, calc)
   else docs

weightAll4Doc :: Int -> Documents -> Documents
weightAll4Doc i docs = let
  nwords = length $ docs ^. _words
  in f nwords docs where
    f 0 docs = docs
    f n docs = f (n-1) $ addWeight (n-1) (i-1) docs

weightAll :: Documents -> Documents
weightAll docs = let
  ndocs = docs ^. _docsN
  in f ndocs docs where
    f 0 docs = docs
    f i docs = f (i-1) $ weightAll4Doc i docs

genVecto2 :: Int -> Documents -> [Int]
genVecto2 i docs = let
  n = docs ^. _words & length
  r = flip map [0..n-1] $ \j -> docs ^. _wordDoc j i _docs
  r' = snd $ unzip r
  in r'

genVector :: Int -> Documents -> [Float]
genVector i docs = let
  n = docs ^. _words & length
  r = flip (parMap rdeepseq) [0..n-1] $ \j -> docs ^. _wordDoc j i _weights
  r' = snd $ unzip r
  in r'

testingstemming :: String -> String
testingstemming l = stem Spanish l

cosineSimilarity :: [Float] -> [Float] -> Float
cosineSimilarity v1 v2 = let
  t = sum $ zipWith (*) v1 v2
  a b = sqrt $ sum $ map (^2) b in
  t / (a v1) * (a v2)

getEveryScorePair :: Documents -> [((Int, Int), Float)]
getEveryScorePair docs = let
  ndocs = docs ^. _docsN
  in [((i, j), 
  cosineSimilarity (genVector i docs) (genVector j docs)) 
  | i <- [0..ndocs - 1], j <- [0..ndocs - 1]]
