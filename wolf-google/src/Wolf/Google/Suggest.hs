{-# LANGUAGE FlexibleContexts #-}

module Wolf.Google.Suggest where

import Import

import Control.Monad.Reader
import Data.Time
import Lens.Micro

import Network.Google.People

import Wolf.Data

import Wolf.Google.Suggest.Adapt
import Wolf.Google.Suggest.Call
import Wolf.Google.Suggest.Finalise
import Wolf.Google.Suggest.Gather
import Wolf.Google.Suggest.MakeEntry
import Wolf.Google.Suggest.Types

suggest :: (MonadIO m, MonadReader DataSettings m) => m ()
suggest = do
    resps <- liftIO getPeople
    now <- liftIO getCurrentTime
    let gatheredPeople =
            flip concatMap resps $ \resp ->
                map gatherData $ resp ^. lcrConnections
    contexts <- getPeopleContexts
    let sugEntries = map (makeSuggestionProperty now) gatheredPeople
    let sugs =
            flip mapMaybe sugEntries $ \pp -> do
                pe <- personEntry pp
                case findSimilar pe contexts of
                    Nothing ->
                        pure
                            EntrySuggestion
                            { entrySuggestionEntry = pe
                            , entrySuggestionLikelyRelevantPerson = Nothing
                            }
                    Just (pc, score) -> do
                        pp' <- adaptToPerson pe pc
                        pe' <- personEntry pp'
                        pure
                            EntrySuggestion
                            { entrySuggestionEntry = pe'
                            , entrySuggestionLikelyRelevantPerson =
                                  Just (personContextUuid pc, score)
                            }
    let finalisedSugs = map finaliseSuggestion sugs
    addPersonEntrySuggestions finalisedSugs

getPeopleContexts ::
       (MonadIO m, MonadReader DataSettings m) => m [PersonContext]
getPeopleContexts = do
    people <- getPersonUuids
    index <- getIndexWithDefault
    forM people $ \uuid -> do
        mpe <- getPersonEntry uuid
        let aliases = map fst $ filter ((== uuid) . snd) $ indexTuples index
        pure
            PersonContext
            { personContextUuid = uuid
            , personContextAliases = aliases
            , personContextEntry = mpe
            }
