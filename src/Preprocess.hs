{-# LANGUAGE LambdaCase, OverloadedStrings, DeriveGeneric #-}

module Preprocess where

import qualified Data.Text as T
import qualified Data.Map as M
import Text.XML.Light (Content (Elem, Text), parseXML, Element (elContent), CData (cdData))
import PrepTexts (parser)
import Data.Maybe (listToMaybe)
import NLP.Stemmer (stem, Stemmer (Spanish))
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()

stemEveryWordOfText :: T.Text -> T.Text
stemEveryWordOfText = T.unwords . map (T.pack . stem Spanish . T.unpack) . T.words

getOnlyElements :: T.Text -> [Content]
getOnlyElements = filter (\case {Elem _ -> True; _ -> False}) . parseXML

data DocumentInfo = DocumentInfo T.Text Int deriving (Show, Generic)

instance Serialize DocumentInfo

getInfoAndData :: [Content] -> [(DocumentInfo, T.Text)]
getInfoAndData (p:n:c:xs) = let
  pname = T.replace " " "" $ readDataFromContent p
  nvalu = read $ T.unpack $ readDataFromContent n
  tdata = stemEveryWordOfText $ parser $ readDataFromContent c in
  (DocumentInfo pname nvalu, tdata) : getInfoAndData xs
getInfoAndData _ = []

readDataFromContent :: Content -> T.Text
readDataFromContent (Elem e) = T.pack $ t $ (listToMaybe $ elContent e) >>= \u -> pure $ extractCData u
  where t = \case {Just x -> x; Nothing -> ""}
readDataFromContent _ = error "Sigfault in readDataFromContent, hehe :D"

extractCData :: Content -> String
extractCData (Text x) = cdData x
extractCData _ = error "Not implemented"

parseDocument :: T.Text -> [(DocumentInfo, T.Text)]
parseDocument = map (\(a,b) -> (a, stemEveryWordOfText b)) . getInfoAndData . getOnlyElements


