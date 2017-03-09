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
