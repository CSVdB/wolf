{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.ExportSpec
    ( spec
    ) where

import TestImport

import Wolf.Data
import Wolf.Data.Export.Types
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
        it "fixes initData" $ \gen ->
            forAll gen $ \sets ->
                uncheckedRepoTest sets $ \repo -> do
                    invalidTimestamp <- generate genValid
                    pure
                        repo
                            { repoInitData =
                                  (repoInitData repo)
                                      {initTimestamp = invalidTimestamp}
                            }

uncheckedRepoTest :: DataSettings -> (Repo -> IO Repo) -> Property
uncheckedRepoTest sets changeRepo =
    forAllValid $ \repo -> do
        badRepo <- changeRepo repo
        repo' <-
            runData sets $ do
                ensureClearRepository
                importRepo badRepo
                runCautiousT exportRepo
        case repo' of
            CautiousError e@(ExportErrorRepoInvalid _) ->
                expectationFailure $
                "Failed with the following error message:\n" ++
                prettyShowExportError e
            _ -> pure ()
