{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Cub where

import Import

import Control.Monad.Reader

import qualified Data.Vector as V

import Brick.AttrMap as A
import Brick.Main
import Brick.Types
import Brick.Util (fg)
import Brick.Widgets.Border as B
import Brick.Widgets.Center as C
import Brick.Widgets.Core
import Brick.Widgets.List
import Graphics.Vty as V

import Wolf.Data.Entry
import Wolf.Data.Index
import Wolf.Data.Types

import Wolf.Cub.OptParse
import Wolf.Cub.Types

runWolfCub :: IO ()
runWolfCub = do
    (DispatchRun RunSettings {..}, Settings) <- getInstructions
    index <- runReaderT getIndexWithDefault runSetDataSettings
    void $ Brick.Main.defaultMain cubApp $ initialState index runSetDataSettings

initialState :: Index -> DataSettings -> CubState
initialState i ds =
    CubState
    { cubStatePersonList = list "person-list" (V.fromList $ indexTuples i) 1
    , cubStatePopupUuid = Nothing
    , cubStateDataSettings = ds
    }

cubApp :: App CubState () ResourceName
cubApp =
    App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }

drawUI :: CubState -> [Widget ResourceName]
drawUI CubState {..} =
    (case cubStatePopupUuid of
         Nothing -> []
         Just personUuid -> [popup personUuid]) ++
    [listUi]
  where
    listUi =
        borderWithLabel (txt "[Wolf Cub]") $
        renderList renderElement True cubStatePersonList
    renderElement :: Bool -> (Text, PersonUuid) -> Widget ResourceName
    renderElement _ (name, _) = padLeftRight 1 $ txt name
    popup (personUuid, mpe) =
        centerLayer $
        borderWithLabel (str $ personUuidString personUuid) $
        case mpe of
            Nothing ->
                let str_ = "No entry found."
                in vLimit 3 $ hLimit (length str_ + 2) $ str str_
            Just pe ->
                let tups = personEntryTuples pe
                    keys = vBox $ map (txt . (<> ": ") . fst) tups
                    values =
                        vBox $
                        map (txt . personPropertyValueContents . snd) tups
                in keys <+> values

theMap :: A.AttrMap
theMap =
    A.attrMap V.defAttr [(listAttr, fg V.white), (listSelectedAttr, fg V.blue)]

appEvent ::
       CubState
    -> BrickEvent ResourceName ()
    -> EventM ResourceName (Next CubState)
appEvent cs e =
    case e of
        (VtyEvent ve) ->
            case cubStatePopupUuid cs of
                Nothing ->
                    case ve of
                        (EvKey V.KEsc []) -> halt cs
                        (EvKey (V.KChar 'q') []) -> halt cs
                        (EvKey V.KEnter []) -> do
                            let msel =
                                    listSelectedElement $ cubStatePersonList cs
                            case msel of
                                Nothing -> continue cs
                                Just (_, (_, personUuid)) -> do
                                    mpe <-
                                        liftIO $
                                        runReaderT (getPersonEntry personUuid) $
                                        cubStateDataSettings cs
                                    continue $
                                        cs
                                        { cubStatePopupUuid =
                                              Just (personUuid, mpe)
                                        }
                        _ -> do
                            nl <- handleListEvent ve $ cubStatePersonList cs
                            continue $ cs {cubStatePersonList = nl}
                Just _ ->
                    let unpop = continue $ cs {cubStatePopupUuid = Nothing}
                    in case ve of
                           (EvKey V.KEsc []) -> unpop
                           (EvKey (V.KChar 'q') []) -> unpop
                           _ -> continue cs
        _ -> continue cs
