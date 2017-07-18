{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Path where

import Import

import qualified Data.Text as T

import Wolf.Data.Types

wolfDir :: MonadReader DataSettings m => m (Path Abs Dir)
wolfDir = asks dataSetWolfDir

initFile :: (MonadReader DataSettings m, MonadIO m) => m (Path Abs File)
initFile = do
    wd <- wolfDir
    liftIO $ resolveFile wd "init.json"

indexFile :: (MonadReader DataSettings m, MonadIO m) => m (Path Abs File)
indexFile = do
    wd <- wolfDir
    liftIO $ resolveFile wd "index.json"

peopleDir :: (MonadReader DataSettings m, MonadIO m) => m (Path Abs Dir)
peopleDir = do
    wd <- wolfDir
    liftIO $ resolveDir wd "people"

personDir ::
       (MonadReader DataSettings m, MonadIO m) => PersonUuid -> m (Path Abs Dir)
personDir personUuid = do
    pd <- peopleDir
    liftIO $ resolveDir pd $ T.unpack $ personUuidText personUuid

personEntryFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
personEntryFile personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveFile pd "entry.json"

tmpPersonEntryFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
tmpPersonEntryFile personUuid = do
    td <- liftIO getTempDir
    liftIO $
        resolveFile td $ T.unpack (personUuidText personUuid) ++ "-entry.wolf"

noteIndexFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> m (Path Abs File)
noteIndexFile personUuid = do
    wd <- personDir personUuid
    liftIO $ resolveFile wd "notes-index.json"

personNotesDir ::
       (MonadReader DataSettings m, MonadIO m) => PersonUuid -> m (Path Abs Dir)
personNotesDir personUuid = do
    pd <- personDir personUuid
    liftIO $ resolveDir pd "notes"

personNoteFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> PersonNoteUuid
    -> m (Path Abs File)
personNoteFile personUuid personNoteUuid = do
    pnd <- personNotesDir personUuid
    liftIO $ resolveFile pnd $ T.unpack $ personNoteUuidText personNoteUuid

tmpPersonNoteFile ::
       (MonadReader DataSettings m, MonadIO m)
    => PersonUuid
    -> PersonNoteUuid
    -> m (Path Abs File)
tmpPersonNoteFile personUuid personNoteUuid = do
    tmpDir <- liftIO getTempDir
    liftIO $
        resolveFile tmpDir $
        T.unpack $
        T.intercalate
            "-"
            [personUuidText personUuid, personNoteUuidText personNoteUuid]
