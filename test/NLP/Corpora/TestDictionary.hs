{-# LANGUAGE OverloadedStrings #-}

module NLP.Corpora.TestDictionary where

import Test.Hspec
import NLP.Corpora.Dictionary


dictionarySpec =
  describe "Add document to dict and doc2bow" $ do
    it "return list of bag of words" $ do
      let document =
            [ "Hooks"
            , "support"
            , "passing"
            , "values"
            , "to"
            , "spec"
            , "items"
            , "to"
            ]
      dict <- new
      dict' <- addDocument document dict
      document_bow <- doc2bow document dict'
      length document_bow `shouldBe` length document - 1
      sum (map snd document_bow) `shouldBe` length document
    it "filterExtremes with unfliterable params stay all without changes" $ do
      let document =
            [ "Hooks"
            , "support"
            , "passing"
            , "values"
            , "to"
            , "spec"
            , "items"
            , "to"
            ]
      dict <- new
      dict' <- addDocument document dict
      dict'' <- filterExtremes 0 (length document) dict'
      document_bow' <- doc2bow document dict'
      document_bow'' <- doc2bow document dict''
      (length document_bow'') `shouldBe` (length document_bow')
