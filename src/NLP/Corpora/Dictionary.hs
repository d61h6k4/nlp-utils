{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-|
This module implements the concept of Dictionary from (gensim <https://radimrehurek.com/gensim/>) project.
-}
module NLP.Corpora.Dictionary
  ( Dictionary
  , new
  , addDocument
  , addDocuments
  , get
  , getToken
  , filterExtremes
  , doc2bow
  ) where

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
data Dictionary = Dictionary
  { token2id :: HashTable.CuckooHashTable Text Int  -- ^ hash table where collect pair of token and tokenid
  , id2token :: HashTable.CuckooHashTable Int Text  -- ^ hash table with tokenid and token pairs (use for restore token by tokenid)
  , currentTokenId :: Int
  , tfs :: HashTable.CuckooHashTable Int Int  -- ^ token frequencies (use for filter too frequent tokens)
  , isChanged :: Bool
  }


-- | Create new dictionary object.
new :: IO Dictionary
new = do
  t2id <- HashTable.new
  id2t <- HashTable.new
  tfs_ <- HashTable.new
  return $
    Dictionary
    {token2id = t2id, id2token = id2t, currentTokenId = -1, tfs = tfs_, isChanged = False}

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
              HashTable.insert (tfs dict') currentTokenId' 1 >>
              return (dict' {currentTokenId = currentTokenId', isChanged = True})
         Just currentTokenId' ->
           HashTable.lookup (tfs dict') currentTokenId' >>= \value ->
             HashTable.insert (tfs dict') currentTokenId' (maybe 1 (+ 1) value) >>
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
  mapM (flip get dict) text >>=
  return .
  Map.toList .
  Map.fromListWith (+) .
  map (\tokenid -> (fromJust tokenid, 1)) . filter isJust . toList


-- | Return token by token id. When no token in dictionary
-- with given id return Nothing.
getToken :: Int -> Dictionary -> IO (Maybe Text)
getToken tokenId dict = do
  _ <-
    case (isChanged dict) of
      True ->
        mapM_ (\(token, tid) -> HashTable.insert (id2token dict) tid token) =<<
        HashTable.toList (token2id dict)
      False -> return ()
  HashTable.lookup (id2token dict) tokenId


-- | Return token id by token. When no token in dictionary
-- with given id return Nothing.
get :: Text -> Dictionary -> IO (Maybe Int)
get token dict = HashTable.lookup (token2id dict) token


-- | Filter out tokens that appear in
--
--   1. less than `no_below` times (absolute number) or
--   2. after (1), keep only the first `keep_n` most frequent tokens (or keep all if `None`).
--
-- After the pruning, shrink resulting gaps in word ids.
--
-- **Note**: Due to the gap shrinking, the same word may have a different
--           word id before and after the call to this function!
filterExtremes :: Int -> Int -> Dictionary -> IO Dictionary
filterExtremes no_below keep_n dict =
  HashTable.toList (tfs dict) >>= return . filter (\(k, v) -> v > no_below) >>=
  return . take keep_n . sortBy (compare `on` snd) >>=
  flip filterOthers dict >>= compactify

filterOthers :: [(Int, Int)] -> Dictionary -> IO Dictionary
filterOthers notOthers Dictionary {..} = do
  let notOthersIds = Set.fromList (map fst notOthers)
  token2id' <-
    HashTable.toList token2id >>=
    return . filter (\(k, v) -> Set.member v notOthersIds) >>=
    HashTable.fromList
  tfs' <- HashTable.fromList notOthers
  return
    (Dictionary
     { token2id = token2id'
     , id2token = id2token
     , tfs = tfs'
     , currentTokenId = currentTokenId
     , isChanged = True
     })

-- | Assign new word ids to all words.
--   This is done to make the ids more compact, e.g. after some tokens have
--   been removed via 'filterExtremes' and there are gaps in the id series.
--   Calling this method will remove the gaps.
compactify :: Dictionary -> IO Dictionary
compactify dict =
  HashTable.toList (token2id dict) >>=
  mapM (\(i, (token, tid)) -> updateTfs tid i >> return (token, i)) . zip [0 ..] >>=
  HashTable.fromList >>= \t2id ->
    return (dict {token2id = t2id, isChanged = True})
  where
    updateTfs :: Int -> Int -> IO ()
    updateTfs tid new_tid =
      HashTable.lookup (tfs dict) tid >>=
      HashTable.insert (tfs dict) new_tid . maybe 0 id
