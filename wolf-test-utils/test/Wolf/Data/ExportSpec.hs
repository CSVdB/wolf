{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.ExportSpec
    ( spec
    ) where

import TestImport

import Wolf.Data
import Wolf.Data.Gen ()

import Wolf.Data.TestUtils

import Cautious.CautiousT

spec :: Spec
spec =
    withDataSetsGen $
    describe "export" $ do
        it "exports 'Nothing' if no wolf repository has been initialised" $ \gen ->
            forAll gen $ \sets -> do
                e <-
                    flip runReaderT sets $ do
                        ensureClearRepository
                        runCautiousT exportRepo
                e `shouldBe` CautiousError NoInitFile
        let roundtrip func name =
                it ("roundtrips the " ++ name) $ \gen ->
                    forAll gen $ \sets ->
                        forAllValid $ \repo -> do
                            repo' <-
                                runData sets $ do
                                    ensureClearRepository
                                    importRepo repo
                                    runCautiousT exportRepo
                            (func <$> repo') `shouldBe`
                                CautiousWarning mempty (func repo)
        -- Re-activate these if necessary for debugging
        -- roundtrip repoInitData "init data"
        -- roundtrip repoPersonIndex "person index"
        -- roundtrip repoNoteIndex "note index"
        -- roundtrip repoNoteIndices "note indices"
        -- roundtrip repoNotes "notes"
        -- roundtrip repoSuggestions "suggestions"
        roundtrip id "entire repo"
