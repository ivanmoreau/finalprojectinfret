{-# LANGUAGE OverloadedStrings, GADTs, TypeFamilies, BlockArguments, DeriveGeneric #-}

module IndexRep where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import Optics
import Data.Maybe (fromMaybe)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Serialize.Text ()

data Documents = Documents {
  __docsN :: Int,
  __docs :: M.Map Int [(Int, Int)],
  __weights :: M.Map Int [(Int, Float)],
  __words :: [T.Text]
} deriving (Show, Generic)

instance Serialize Documents

_docs :: Lens 
  Documents Documents
  (M.Map Int [(Int, Int)]) (M.Map Int [(Int, Int)])
_docs = lens __docs $ \s x -> s { __docs = x }

_weights :: Lens
  Documents Documents
  (M.Map Int [(Int, Float)]) (M.Map Int [(Int, Float)])
_weights = lens __weights $ \s x -> s { __weights = x }

_docsN :: Lens
  Documents Documents
  Int Int
_docsN = lens __docsN $ \s x -> s { __docsN = x }

_words :: Lens
  Documents Documents
  [T.Text] [T.Text]
_words = lens __words $ \s x -> s { __words = x }

_word :: Int -> Lens Documents Documents
  (M.Map Int [(Int, a)]) (M.Map Int [(Int, a)])
  -> Lens Documents Documents
  [(Int, a)] [(Int, a)]
_word i o = lens a b where
  a e = e ^. o ^. at i & fromMaybe []
  b e n = let
    r = e ^. o
    r' = r & at i ?~ n in e & o .~ r'

_wordDoc :: Num a => Int -> Int -> Lens Documents Documents
  (M.Map Int [(Int, a)]) (M.Map Int [(Int, a)])
  -> Lens Documents Documents
  (Int, a) (Int, a)
_wordDoc i j o = lens a b where
  lin = _word i o 
  a e = (\x -> (j, x)) $ e ^. lin & 
    L.lookup j & fromMaybe 0
  b e n = let
    r = e ^. lin
    r' = r & L.lookup j
    r'' = fromMaybe r $ r' >>= \_ -> pure $ 
      L.deleteBy (\(z,_) (x,_)->z==x) (j,0) r in
    e & lin .~ (n:r'')

emptyDocuments :: Documents
emptyDocuments = 
  Documents 0 (M.fromList []) (M.fromList []) []

cleanDocS :: T.Text -> [T.Text]
cleanDocS = L.nub .
  (filter (\x -> x /= "" || x /= " ")) . T.words

cleanDoc :: T.Text -> [T.Text]
cleanDoc =
  (filter (\x -> x /= "" || x /= " ")) . T.words

modifyDoc :: (Int, Int) -> Int -> Documents -> Documents
modifyDoc (i, f) index d = d & _docs %~ fun where
  fun m = m & at index %~ kun
  kun p = p >>= \p -> pure $ (i, f) : p

tf :: Int -> Int -> Documents -> Float
tf word doc docs = let
  tftd = fromIntegral $ docs ^. _docs & M.lookup word & 
    fromMaybe 0 . flip (>>=) \a -> L.lookup doc a
  in if tftd > 0 then 1 + logBase 2 tftd else 0

idf :: Int -> Documents -> Float
idf word doc = let
  n = fromIntegral $ doc ^. _docsN
  d = fromIntegral $ length $ doc ^. _docs & 
    M.lookup word & fromMaybe []
  in logBase 2 $ n / d

tfidf :: Int -> Int -> Documents -> Float
tfidf word doc docs =
  tf word doc docs * idf word docs

addWord :: T.Text -> Int -> Documents -> Documents
addWord w doc docs = let
  i = fromMaybe (-1) $ docs ^. _words & 
    L.findIndex (w==)
  wordI = docs & _wordDoc i doc _docs %~ f
  in wordI where
    f (i, j) = (i, j + 1)

addDoc :: T.Text -> Int -> Documents -> Documents
addDoc w doc docs = let
  wws = cleanDoc w in f wws doc docs where
  f [] _ d = d
  f (w:ws) i d = f ws i $ addWord w i d

doDocs :: [T.Text] -> Documents -> Documents
doDocs t d = f 0 t d where
  f _ [] d = d
  f i (w:ws) d = f (i + 1) ws $ addDoc w i d

prepareDoc :: [T.Text] -> Documents
prepareDoc t = let
  a = length t
  b = mconcat t
  c = cleanDocS b
  g = length c
  d = emptyDocuments {
    __docsN = a,
    __words = L.sort c,
    __docs = M.fromList $ zip [0..g - 1] $ repeat [],
    __weights = M.fromList $ zip [0..g - 1] $ repeat []
  } in d


