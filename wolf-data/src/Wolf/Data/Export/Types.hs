{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types
    ( Repo(..)
    , CautiousS
    , cautiousProblem
    , cautiousProblemM
    , cautiousProblemIfNothing
    , prettyShowWarn
    , prettyShowErr
    , Warn
    , Problem(..)
    , Err(..)
    ) where

import Import

import Data.Aeson
import qualified Data.Map as M

import qualified Data.Set as S

import Cautious.CautiousT

import Wolf.Data.Entry.Types
import Wolf.Data.Index
import Wolf.Data.Init.Types
import Wolf.Data.Note.Types
import Wolf.Data.NoteIndex.Types
import Wolf.Data.People.Types
import Wolf.Data.Suggestion.Types

data Repo = Repo
    { repoInitData :: InitData
    , repoPersonIndex :: Index
    , repoPersonEntries :: Map PersonUuid PersonEntry
    , repoNoteIndex :: NoteIndex
    , repoNoteIndices :: Map PersonUuid NoteIndex
    , repoNotes :: Map NoteUuid Note
    , repoSuggestions :: SuggestionRepo
    } deriving (Show, Eq, Generic)

instance Validity Repo where
    validate Repo {..} =
        mconcat
            [ repoInitData <?!> "repoInitData"
            , repoPersonIndex <?!> "repoPersonIndex"
            , repoPersonEntries <?!> "repoPersonEntries"
            , repoNoteIndex <?!> "repoNoteIndex"
            , repoNoteIndices <?!> "repoNoteIndices"
            , repoNotes <?!> "repoNotes"
            , M.keysSet repoNotes ==
              noteIndexSet repoNoteIndex <?@>
              "The key set of repoNotes equals the note UUID's in the global note index."
            , mconcat $
              flip map (M.toList repoNoteIndices) $ \(personUuid, noteIndex) ->
                  noteIndex `isSubNoteIndexOf` repoNoteIndex <?@>
                  unlines
                      [ "The person note index for person"
                      , uuidString personUuid
                      , "is a sub-noteindex of the global note index."
                      , "Person note index: " ++ show noteIndex
                      , "Global note index: " ++ show repoNoteIndex
                      ]
            , repoSuggestions <?!> "repoSuggestions"
            , mconcat $
              itsNotesMentionPerson repoNoteIndices repoNotes <$>
              (snd <$> indexTuples repoPersonIndex) -- If pu refers to nu, nu refers to pu
            , mconcat $
              itsPeopleMentionNote repoNoteIndices repoNotes <$>
              S.toList (noteIndexSet repoNoteIndex)
            -- If nu refers to pu, pu refers to nu
            ]
    isValid = isValidByValidating

getPersonNotes :: Map PersonUuid NoteIndex -> PersonUuid -> [NoteUuid]
getPersonNotes noteIndices pu =
    case M.lookup pu noteIndices of
        Nothing -> []
        Just ni -> S.toList $ noteIndexSet ni

getNotePeople :: Map NoteUuid Note -> NoteUuid -> [PersonUuid]
getNotePeople noteMap nu =
    case M.lookup nu noteMap of
        Nothing -> []
        Just Note {..} -> S.toList noteRelevantPeople

itsNotesMentionPerson ::
       Map PersonUuid NoteIndex -> Map NoteUuid Note -> PersonUuid -> Validation
itsNotesMentionPerson noteIndices noteMap pu =
    mconcat $ isRelevantTo pu noteMap <$> getPersonNotes noteIndices pu

itsPeopleMentionNote ::
       Map PersonUuid NoteIndex -> Map NoteUuid Note -> NoteUuid -> Validation
itsPeopleMentionNote noteIndices noteMap nu =
    mconcat $ isMentionedBy nu noteIndices <$> getNotePeople noteMap nu

isMentionedBy ::
       NoteUuid -> Map PersonUuid NoteIndex -> PersonUuid -> Validation
isMentionedBy nu noteIndices pu =
    check (elem nu $ getPersonNotes noteIndices pu) $
    mconcat
        [ "Note "
        , show nu
        , " mentions person "
        , show pu
        , ", so the person refers to the note."
        ]

isRelevantTo :: PersonUuid -> Map NoteUuid Note -> NoteUuid -> Validation
isRelevantTo pu noteMap nu =
    check (elem pu $ getNotePeople noteMap nu) $
    mconcat
        [ "Person "
        , show pu
        , " refers to note "
        , show nu
        , ", so the note refers to the person."
        ]

instance NFData Repo

instance FromJSON Repo where
    parseJSON =
        withObject "Repo" $ \o ->
            Repo <$> o .: "init-data" <*> o .: "person-index" <*>
            o .: "person-entries" <*>
            o .: "note-index" <*>
            o .: "note-indices" <*>
            o .: "notes" <*>
            o .: "suggestions"

instance ToJSON Repo where
    toJSON Repo {..} =
        object
            [ "init-data" .= repoInitData
            , "person-index" .= repoPersonIndex
            , "person-entries" .= repoPersonEntries
            , "note-index" .= repoNoteIndex
            , "note-indices" .= repoNoteIndices
            , "notes" .= repoNotes
            , "suggestions" .= repoSuggestions
            ]

type Warn = [Problem]

data Problem
    = WarnMissingNoteIndex PersonUuid
                           NoteUuid
    | WarnMissingNote NoteUuid
    | WarnMissingRelevantPerson PersonUuid
                                NoteUuid
    | WarnMissingRelevantNote NoteUuid
                              PersonUuid
    deriving (Show, Eq, Generic)

instance ToJSON Problem

instance FromJSON Problem

data Err =
    NoInitFile
    deriving (Show, Eq, Generic)

instance ToJSON Err

instance FromJSON Err

type CautiousS = CautiousT Warn Err

cautiousProblem :: Monad m => Problem -> a -> CautiousS m a
cautiousProblem p = cautiousWarning [p]

cautiousProblemIfNothing ::
       Monad m => Problem -> Maybe a -> CautiousS m (Maybe a)
cautiousProblemIfNothing p = cautiousWarningIfNothing [p]

cautiousProblemM :: Monad m => Problem -> m a -> CautiousS m a
cautiousProblemM p = cautiousWarningM [p]

prettyShowErr :: Err -> String
prettyShowErr NoInitFile =
    "Error: the wolf repository is not or poorly initialised."

prettyShowProblem :: Problem -> String
prettyShowProblem (WarnMissingNoteIndex pu nu) =
    mconcat
        [ "Warning: "
        , show pu
        , " has no NoteIndex, but needs to have one since note "
        , show nu
        , " mentions him/her."
        ]
prettyShowProblem (WarnMissingNote nu) =
    "Warning: " ++ show nu ++ " has no note."
prettyShowProblem (WarnMissingRelevantPerson pu nu) =
    "Warning: The note " ++
    show nu ++ " does not mention the person " ++ show pu ++ "."
prettyShowProblem (WarnMissingRelevantNote nu pu) =
    "Warning: The person " ++
    show pu ++ " does not mention the note " ++ show nu ++ " in his noteIndex."

prettyShowWarn :: Warn -> String
prettyShowWarn [] = "Everything succeeded!"
prettyShowWarn xs = intercalate "\n" $ prettyShowProblem <$> xs
