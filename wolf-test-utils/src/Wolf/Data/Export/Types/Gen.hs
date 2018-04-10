{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types.Gen where

import Import

import qualified Data.Map as M
import qualified Data.Set as S

import Wolf.Data.Export.Types
import Wolf.Data.Index.Types
import Wolf.Data.Note.Types
import Wolf.Data.NoteIndex.Types
import Wolf.Data.People.Types

import Wolf.Data.Index.Types.Gen ()
import Wolf.Data.Init.Types.Gen ()
import Wolf.Data.Note.Types.Gen ()
import Wolf.Data.NoteIndex.Types.Gen
import Wolf.Data.People.Types.Gen ()
import Wolf.Data.Suggestion.Types.Gen ()

import Cautious.Cautious

instance GenUnchecked Repo

instance GenValid Repo where
    genValid = do
        repoInitData <- genValid
        -- Any list of persons
        eps <- genValid
        -- The person index consists of (some) people from the global index
        -- and some occur multiple times.
        repoPersonIndex <-
            let gen ix puuid = do
                    a <- genValid
                    go (addIndexEntry a puuid ix) puuid
                go ix puuid = frequency [(3, gen ix puuid), (1, pure ix)]
            in foldM go newIndex eps
        -- For some people, make a person entry.
        repoPersonEntries <-
            fmap (M.fromList . catMaybes) $
            forM eps $ \p -> oneof [pure Nothing, (Just . (,) p) <$> genValid]
        -- Any global note index.
        repoNoteIndex <- genValid
        let noteUuids = S.toList $ noteIndexSet repoNoteIndex
        -- For some people, make a note index with a sub note index of the global note index.
        repoNoteIndices <-
            fmap (M.fromList . catMaybes) $
            forM eps $ \p -> do
                b <- genValid
                if b
                    then (Just . (,) p) <$> subNoteIndex repoNoteIndex
                    else pure Nothing
        -- For each noteuuid, make a note.
        repoNotesWithWrongPeople <-
            fmap M.fromList $
            forM noteUuids $ \nu -> (,) nu . rmRelevantPeople <$> genValid
        let repoNotes =
                foldl'
                    updateNotes
                    repoNotesWithWrongPeople
                    (M.toList repoNoteIndices)
        repoSuggestions <- genValid
        pure Repo {..}

rmRelevantPeople :: Note -> Note
rmRelevantPeople note = note {noteRelevantPeople = S.empty}

updateNotes :: Map NoteUuid Note -> (PersonUuid, NoteIndex) -> Map NoteUuid Note
updateNotes noteMap (pu, ni) =
    let noteUuids = S.toList $ noteIndexSet ni
    in foldl' (addRelevantPerson pu) noteMap noteUuids

addRelevantPerson ::
       PersonUuid -> Map NoteUuid Note -> NoteUuid -> Map NoteUuid Note
addRelevantPerson pu noteMap nu =
    M.adjust
        (\note ->
             note {noteRelevantPeople = S.insert pu $ noteRelevantPeople note})
        nu
        noteMap

instance (GenUnchecked a, GenUnchecked w, GenUnchecked e) =>
         GenUnchecked (Cautious w e a)

instance (GenValid a, GenValid w, GenValid e) => GenValid (Cautious w e a)

instance Validity Problem

instance GenUnchecked Problem

instance GenValid Problem

instance Validity Err

instance GenUnchecked Err

instance GenValid Err
