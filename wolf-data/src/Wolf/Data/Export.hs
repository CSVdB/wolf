{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Export
    ( Repo
    , repoInitData
    , repoPersonIndex
    , exportRepo
    , CautiousS
    , prettyShowWarn
    , prettyShowErr
    , Warn
    , Problem(..)
    , Err(..)
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

exportRepo :: (MonadIO m, MonadReader DataSettings m) => CautiousS m Repo
exportRepo = do
    mid <- lift readInitData
    initData <- cautiousErrorIfNothing mid NoInitFile
    mi <- lift getIndexWithDefault
    people <- lift getPersonUuids -- These are collected from the people directory
    entries <- lift $ mKeyed getPersonEntry people
    noteIndex <- lift getNoteIndex
    mNoteIxs <- getMapCautious getPersonNoteIndexCautious people
    noteUuids <- lift getNoteUuids
    notes <- getMapCautious readNoteCautious noteUuids
    sugs <- lift readAllSuggestions
    pure
        Repo
        { repoInitData = initData
        , repoPersonIndex = mi
        , repoPersonEntries = entries
        , repoNoteIndex = noteIndex
        , repoNoteIndices = mNoteIxs
        , repoNotes = notes
        , repoSuggestions = sugs
        }
  where
    mKeyed :: (Ord a, Monad m) => (a -> m (Maybe b)) -> [a] -> m (Map a b)
    mKeyed func ls =
        (M.fromList . mapMaybe (\(p, e) -> (,) p <$> e)) <$>
        mapM (\p -> (,) p <$> func p) ls
    getMapCautious ::
           (Ord a, Monad m)
        => (a -> CautiousS m (Maybe b))
        -> [a]
        -> CautiousS m (Map a b)
    getMapCautious func ls =
        M.fromList . catMaybes . fmap sequence <$>
        traverse (\a -> (,) a <$> func a) ls
