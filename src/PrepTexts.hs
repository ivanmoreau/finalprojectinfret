{- |
Copyright: (c) 2022 Ivan Molina Rebolledo
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Ivan Molina Rebolledo <ivanmolinarebolledo@gmail.com>
See README for more info
-}

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module PrepTexts
       ( 
         parser
       ) where

import Data.Text (Text, pack, concat, replace, toLower, unpack, unwords, words)
import Stopwords (stopwords)
import Text.Parsec ((<|>), many, many1, option, optionMaybe, try, parse)
import Text.Parsec.Char (alphaNum, char, digit, letter, oneOf, string)
import Text.Parsec.Text (Parser, parseFromFile)
import Text.Parsec.Error (ParseError) 
import Control.Parallel.Strategies (rdeepseq, ($||), parList, withStrategy)
import Text.RE.TDFA.Text (RE, re, (*=~/), SearchReplace (SearchReplace))

spaces :: Parser Text
spaces = many1 (char ' ') >> return " "

pmarks :: Parser Text
pmarks = oneOf "()¿?!¡;:,.'\'\"[]{}-_@#<>«»&…\\/—►| " >> return " "

pmarkslist :: [Text]
pmarkslist = ["(", ")", "¿", "¡", ";", ",", ":", ".", "'", "\"", "[", "]", "{",
  "}", "-", "_", "@", "#", "<", ">", "«", "»", "&", "…", "\\", "/", "—", "►",
  "|", "+", "=", "*", "%", "^", "~", "`", "´", "’", "‘", "“", "”", "„", "‹",
  "–", "—", "›", "“", "”", "„", "‹", "–", "—", "›", "“", "”", "„", "‹", "–", "?", "!", "®"]

replaceAll :: [Text] -> Text -> Text -> Text
replaceAll [] _ t = t
replaceAll (x:xs) y t = (\a -> ($||) a rdeepseq) (replaceAll xs y) (replace x y t)

replacepmarks :: Text -> Text
replacepmarks = replaceAll pmarkslist ""

wordWS :: Parser Text
wordWS = letter >>= \c -> many letter >>= \x -> 
       return (case (elem (unpack (toLower (pack (c:x)))) stopwords) of
              True -> pack ""
              False -> toLower (pack (c:x)))

newline :: Parser Text
newline = string "\n" >> return "\n"

numbers :: Parser Text
numbers = digit >>= \_ -> return ""

replaceNumbers :: Text -> Text
replaceNumbers = replaceAll ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] ""

parse_ :: Parser Text
parse_ =  ( many (try spaces <|> try wordWS <|> try pmarks 
       <|> try numbers <|> try newline ) ) 
       >>= \x -> return (replaceSpaces (Data.Text.concat x))


urlRegex :: RE
urlRegex = [re|https?[a-zA-Z]+|]

deleteURLs :: Text -> Text
deleteURLs = flip (*=~/) $ SearchReplace urlRegex ""

replaceSpaces :: Text -> Text
replaceSpaces l = rrep 1 l

rrep :: Int -> Text -> Text
rrep 1 l = l
rrep n l = rrep (n - 1) (replace (pack (replicate n ' ')) (pack " ") l)

replaceAllNotWords :: Text -> Text
replaceAllNotWords = deleteURLs . replacepmarks . replaceNumbers

parFilter strat f = withStrategy (parList strat) . filter f

filterStopwords :: [Text] -> [Text]
filterStopwords = filter (\x -> not (elem (unpack x) stopwords)) 

parser :: Text -> Text
parser = Data.Text.unwords . filterStopwords . Data.Text.words . toLower . replaceAllNotWords
_parser file = case (parse parse_ "" file) of
  Left e -> error $ show $ e
  Right x -> x
