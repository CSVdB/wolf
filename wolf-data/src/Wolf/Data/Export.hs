{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Export
    ( Repo
    , repoInitData
    , repoPersonIndex
    , exportRepo
    , CautiousExport
    , prettyShowExportWarning
    , prettyShowExportError
    , ExportWarning
    , ExportProblem(..)
    , ExportError(..)
    ) where

import Import

import qualified Data.Map as M

import Wolf.Data.Cautious
import Wolf.Data.Export.Types
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.NoteIndex
import Wolf.Data.People
import Wolf.Data.Suggestion
import Wolf.Data.Types

import Cautious.CautiousT

getNoteMap :: MonadIO m => [NoteUuid] -> m (Map NoteUuid Note)
getNoteMap = undefined

getPeopleMap :: MonadIO m => [PersonUuid] -> m (Map PersonUuid NoteIndex)
getPeopleMap = undefined

stuff ::
       Result ~ (Map NoteUuid Note, Map PersonUuid NoteIndex)
    => Result
    -> CautiousExport m Result
stuff = undefined

exportRepo :: (MonadIO m, MonadReader DataSettings m) => CautiousExport m Repo
exportRepo = do
    mid <- lift readInitData
    initData <- cautiousErrorIfNothing mid NoInitFile
    mi <- lift getIndexWithDefault
    people <- lift getPersonUuids -- These are collected from the people directory
    entries <- lift $ mKeyed getPersonEntry people
    noteIndex <- lift getNoteIndex
    noteUuids <- lift getNoteUuids
    unsafeNoteMap <- getNoteMap noteUuids
    unsafePersonmap <- getPersonMap people
    let (mNoteIxs, notes) = stuff (unsafeNoteMap, people)
    sugs <- lift readAllSuggestions
    let uncheckedRepo =
            Repo
                { repoInitData = initData
                , repoPersonIndex = mi
                , repoPersonEntries = entries
                , repoNoteIndex = noteIndex
                , repoNoteIndices = mNoteIxs
                , repoNotes = notes
                , repoSuggestions = sugs
                }
    case eitherInvalidRepoMessage uncheckedRepo of
        Left err -> cautiousError $ ExportErrorRepoInvalid err
        Right repo -> pure repo
  where
    mKeyed :: (Ord a, Monad m) => (a -> m (Maybe b)) -> [a] -> m (Map a b)
    mKeyed func ls =
        (M.fromList . mapMaybe (\(p, e) -> (,) p <$> e)) <$>
        mapM (\p -> (,) p <$> func p) ls
    getMapCautious ::
           (Ord a, Monad m)
        => (a -> CautiousExport m (Maybe b))
        -> [a]
        -> CautiousExport m (Map a b)
    getMapCautious func ls =
        M.fromList . catMaybes . fmap sequence <$>
        traverse (\a -> (,) a <$> func a) ls
