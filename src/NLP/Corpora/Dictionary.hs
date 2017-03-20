{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-|
This module implements the concept of Dictionary from (gensim <https://radimrehurek.com/gensim/>) project.
-}
module NLP.Corpora.Dictionary where

import Data.List (sortBy)
import Data.Function (on)
import Data.Foldable (toList)
import Data.Maybe (isJust, fromJust)
import Control.Monad (foldM, mapM)
import Data.Text (Text)

import qualified Data.HashTable.IO as HashTable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- | Main structure containing
-- `token2id` hash table where collect pair of token and tokenid
-- `id2token` hash table with tokenid and token pairs (use for restore token by tokenid)
-- `dfs` is document frequencies (use for filter too frequent tokens)
data Dictionary = Dictionary
  { token2id :: HashTable.CuckooHashTable Text Int
  , id2token :: HashTable.CuckooHashTable Int Text
  , currentTokenId :: Int
  , dfs :: HashTable.CuckooHashTable Int Int
  }


-- | Create new dictionary object.
new :: IO Dictionary
new = do
  t2id <- HashTable.new
  id2t <- HashTable.new
  dfs_ <- HashTable.new
  return $
    Dictionary
    {token2id = t2id, id2token = id2t, currentTokenId = 0, dfs = dfs_}

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
              HashTable.insert (dfs dict') currentTokenId' 1 >>
              return (dict' {currentTokenId = currentTokenId'})
         Just currentTokenId' ->
           HashTable.lookup (dfs dict') currentTokenId' >>= \value ->
             HashTable.insert (dfs dict') currentTokenId' (maybe 1 (+ 1) value) >>
             return dict')
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


-- | Filter out tokens that appear in
--
-- 1. less than `no_below` documents (absolute number) or
-- 2. more than `no_above` documents (absolute number, because we can't count
--    how many documents stay after filter extremes).
-- 3. after (1) and (2), keep only the first `keep_n` most frequent tokens (or keep all if `None`).
--
-- After the pruning, shrink resulting gaps in word ids.
--
-- **Note**: Due to the gap shrinking, the same word may have a different
-- word id before and after the call to this function!
filterExtremes :: Int -> Int -> Int -> Dictionary -> IO Dictionary
filterExtremes no_below no_above keep_n dict =
  HashTable.toList (dfs dict) >>=
  return . filter (\(k, v) -> v > no_below && v < no_above) >>=
  return . take keep_n . sortBy (compare `on` snd) >>=
  flip filterOthers dict

filterOthers :: [(Int, Int)] -> Dictionary -> IO Dictionary
filterOthers notOthers Dictionary {..} = do
  let notOthersIds = Set.fromList (map fst notOthers)
  token2id' <-
    HashTable.toList token2id >>=
    return . filter (\(k, v) -> Set.member v notOthersIds) >>=
    HashTable.fromList
  id2token' <-
    HashTable.toList id2token >>=
    return . filter (\(k, v) -> Set.member k notOthersIds) >>=
    HashTable.fromList
  dfs' <- HashTable.fromList notOthers
  return
    (Dictionary
     { token2id = token2id'
     , id2token = id2token'
     , dfs = dfs'
     , currentTokenId = currentTokenId
     })
