module Wolf.Data
    ( DataSettings(..)
    -- * Init
    , InitData(..)
    , readInitData
    , initWolf
    , withInitCheck
    , withInitCheck_
    -- * Global index
    , PersonUuid
    , getPersonUuids
    , Alias
    , alias
    , aliasText
    , aliasString
    , Index
    , indexMap
    , indexKeys
    , indexTuples
    , lookupInIndex
    , reverseIndex
    , reverseIndexSingleAlias
    , reverseIndexLookup
    , reverseIndexLookupSingleAlias
    , addIndexEntry
    , createNewPerson
    , addAliases
    , addAlias
    , lookupOrCreateNewPerson
    , getIndex
    , getIndexWithDefault
    , putIndex
    -- * Person Entries
    , PersonEntry
    , personEntry
    , personEntryProperties
    , sameProperties
    , newPersonEntry
    , PersonProperty(..)
    , sameValues
    , PersonPropertyValue(..)
    , sameContents
    , entryContents
    , entryContentsBS
    , updatePersonEntry
    , UpdateResult(..)
    , EntryParseException
    , prettyPrintEntryParseException
    , getPersonEntry
    , putPersonEntry
    , deletePersonEntry
    -- * Notes
    , NoteUuid
    , NoteIndex
    , noteIndexSet
    , newNoteIndex
    , addToNoteIndex
    , containsNoteUuid
    , isSubNoteIndexOf
    -- ** Global note index
    , getNoteIndex
    , putNoteIndex
    , getNoteUuids
    , getNotes
    -- ** Person note index
    , getPersonNoteIndex
    , putPersonNoteIndex
    , getPersonNoteUuids
    , getPersonNotes
    -- ** Notes
    , Note(..)
    , createNewNote
    , createNewNoteUuid
    , readNote
    , readNoteCautious
    , writeNote
    -- * Suggestions
    , SuggestionType(..)
    , parseSuggestionType
    , SuggestionUuid
    , Suggestion(..)
    , aliasSuggestionType
    , AliasSuggestion(..)
    , entrySuggestionType
    , EntrySuggestion(..)
    , sameEntrySuggestionData
    , sameEntrySuggestion
    , readSuggestion
    , readUnusedSuggestions
    , addUnusedSuggestions
    , addUnusedSuggestion
    , readUsedSuggestions
    , recordUsedSuggestions
    , recordUsedSuggestion
    , SuggestionRepo
    , SuggestionTypeRepo
    , readAllSuggestions
    , writeAllSuggestions
    , Agreement(..)
    , parseAgreement
    , renderAgreement
    -- * Import and Export
    , Repo
    , repoInitData
    , repoPersonIndex
    , exportRepo
    , importRepo
    , cleanupRepo
    , CautiousExport
    , prettyShowExportWarning
    , prettyShowExportError
    , ExportWarning
    , ExportProblem(..)
    , ExportError(..)
    , getPersonNoteIndexCautious
    ) where

import Wolf.Data.Cautious
import Wolf.Data.Cleanup
import Wolf.Data.Entry
import Wolf.Data.Export
import Wolf.Data.Import
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.People
import Wolf.Data.Suggestion
import Wolf.Data.Types
