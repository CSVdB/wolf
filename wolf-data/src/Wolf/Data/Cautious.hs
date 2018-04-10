{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Cautious
    ( getPersonNoteIndexCautious
    , readNoteCautious
    ) where

import Import

import qualified Data.Set as S

import Wolf.Data.Export.Types
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.People
import Wolf.Data.Types

-- Checks whether a given note mentions a given person as 'relevant'
checkNotePersonRelation ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> NoteUuid
    -> CautiousS m ()
checkNotePersonRelation pu nu = do
    maybeNote <- lift $ readNote nu
    case maybeNote of
        Nothing -> cautiousProblem (WarnMissingNote nu) ()
        Just note ->
            if S.member pu $ noteRelevantPeople note
                then pure ()
                else cautiousProblem (WarnMissingRelevantPerson pu nu) ()

getPersonNoteIndexCautious ::
       (MonadIO m, MonadReader DataSettings m)
    => PersonUuid
    -> CautiousS m (Maybe NoteIndex)
getPersonNoteIndexCautious pu = do
    noteIndex <- lift $ getPersonNoteIndex pu
    case noteIndex of
        Nothing -> pure Nothing
        Just ni -> do
            forM_ (toList $ noteIndexSet ni) $ checkNotePersonRelation pu
            pure $ Just ni

-- Checks whether a given person has a given note listed in its noteIndex
checkPersonNoteRelation ::
       (MonadIO m, MonadReader DataSettings m)
    => NoteUuid
    -> PersonUuid
    -> CautiousS m ()
checkPersonNoteRelation nu pu = do
    maybePersonNoteIndex <- lift $ getPersonNoteIndex pu
    case maybePersonNoteIndex of
        Nothing -> cautiousProblem (WarnMissingNoteIndex pu nu) ()
        Just ni ->
            if S.member nu $ noteIndexSet ni
                then pure ()
                else cautiousProblem (WarnMissingRelevantNote nu pu) ()

readNoteCautious ::
       (MonadIO m, MonadReader DataSettings m)
    => NoteUuid
    -> CautiousS m (Maybe Note)
readNoteCautious nu = do
    maybeNote <- lift $ readNote nu
    case maybeNote of
        Nothing -> cautiousProblem (WarnMissingNote nu) Nothing
        Just note -> do
            forM_ (toList $ noteRelevantPeople note) $
                checkPersonNoteRelation nu
            pure $ Just note
