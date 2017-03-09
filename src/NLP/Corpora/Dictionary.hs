{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-|
This module implements the concept of Dictionary from (gensim <https://radimrehurek.com/gensim/>) project.
-}
module NLP.Corpora.Dictionary where

import Data.Foldable (toList)
import Data.Maybe (isJust, fromJust)
import Control.Monad (foldM, mapM)
import Data.Text (Text)

import qualified Data.HashTable.IO as HashTable
import qualified Data.Map.Strict as Map


data Dictionary = Dictionary
  { token2id :: HashTable.CuckooHashTable Text Int
  , id2token :: HashTable.CuckooHashTable Int Text
  , currentTokenId :: Int
  }


-- | Create new dictionary object.
new :: IO Dictionary
new = do
  t2id <- HashTable.new
  id2t <- HashTable.new
  return $ Dictionary {token2id = t2id, id2token = id2t, currentTokenId = 0}

-- | Add document to dictionary. Insert all new unique tokens
-- from document to dictionary. Document is the foldable (e.g. list)
-- container of tokens.
addDocument
  :: (Foldable f)
  => f Text -> Dictionary -> IO Dictionary
addDocument text dict =
  foldM
    (\dict' token ->
       HashTable.lookup (token2id dict') token >>= \case
         Nothing ->
           let currentTokenId' = currentTokenId dict' + 1
           in HashTable.insert (token2id dict') token currentTokenId' >>
              HashTable.insert (id2token dict') currentTokenId' token >>
              return (dict' {currentTokenId = currentTokenId'})
         Just _ -> return dict')
    dict
    text


-- | Add documents to dictionary.
addDocuments
  :: (Foldable f, Foldable g)
  => f (g Text) -> Dictionary -> IO Dictionary
addDocuments texts dict =
  foldM (\dict' text -> addDocument text dict') dict texts


-- | Convert document into the bag of words format.
-- Bag of words format is list of tuples of token id and token count.
doc2bow
  :: (Traversable f)
  => f Text -> Dictionary -> IO [(Int, Int)]
doc2bow text dict =
  mapM (HashTable.lookup (token2id dict)) text >>=
  return .
  Map.toList .
  Map.fromListWith (+) .
  map (\tokenid -> (fromJust tokenid, 1)) . filter isJust . toList


-- | Return token by token id. When no token in dictionary
-- with given id return Nothing.
get :: Int -> Dictionary -> IO (Maybe Text)
get tokenId dict = HashTable.lookup (id2token dict) tokenId
