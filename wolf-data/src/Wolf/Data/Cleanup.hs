{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Cleanup
    ( cleanupRepo
    ) where

import Import

import Wolf.Data.Export
import Wolf.Data.Import
import Wolf.Data.Types

import Wolf.Data.Prompt

import Cautious.Cautious
import Cautious.CautiousT

cleanupRepo :: (MonadIO m, MonadReader DataSettings m) => m ()
cleanupRepo = do
    mr <- runCautiousT exportRepo
    case mr of
        CautiousError e -> liftIO . die $ prettyShowErr e
        CautiousWarning [] repo -> importRepo repo
        CautiousWarning w repo -> do
            liftIO . putStrLn $ prettyShowWarn w
            answer <-
                liftIO $
                promptYesNo No "Do you want to clean up the repository anyway?"
            when (answer == Yes) $ importRepo repo
