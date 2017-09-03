{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Cub.PropertyEditor
    ( PropertyEditor
    , propertyEditor
    , renderPropertyEditor
    , handlePropertyEditorEvent
    , propertyEditorAttr
    , propertyEditorAttrSelected
    ) where

import Import

import qualified Data.Text as T
import Data.Time

import Safe

import Graphics.Vty as Vty
import Graphics.Vty as V

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit

import Wolf.Data

import Wolf.Cub.PropertyEditor.Cursor

data PropertyEditor n = PropertyEditor
    { propertyEditorName :: n
    , propertyEditorCursor :: Maybe ACursor
    , propertyEditorCurrentEditor :: Maybe (Editor Text n)
    } deriving (Generic)

propertyEditor :: n -> Maybe PersonProperty -> PropertyEditor n
propertyEditor name mprop =
    PropertyEditor
    { propertyEditorName = name
    , propertyEditorCursor = cursor <$> mprop
    , propertyEditorCurrentEditor = Nothing
    }

renderPropertyEditor :: (Show n, Ord n) => PropertyEditor n -> Widget n
renderPropertyEditor PropertyEditor {..}
    -- (withAttr
    --      propertyEditorAttrSelected
    --      (case propertyEditorCursor of
    --           Nothing -> emptyWidget
    --           Just cur ->
    --               str $
    --               case cur of
    --                   APropC pc -> show $ build pc
    --                   ALElC lec -> show $ build lec
    --                   AKC kc -> show $ build kc
    --                   AMKVC kvc -> show $ build kvc) <=>) $ -- TODO remove this.
    -- ((withAttr
    --       propertyEditorAttrSelected
    --       (str (show $ propertyEditorSelection <$> cur)) <=>
    --   str " ") <=>) $ -- TODO remove this.
    -- (<=> withAttr
    --          propertyEditorAttrSelected
    --          (strWrap
    --               (show $
    --                select propertyEditorSelection <$>
    --                (rebuild <$> propertyEditorCursor)))) $ -- TODO remove this.
 =
    withAttr propertyEditorAttr $
    vBox
        [ case rebuild <$> propertyEditorCursor of
              Nothing -> txt "No properties, press 's' to start a new property."
              Just pp ->
                  padRight Max $
                  padBottom Max $ go (makeSelection <$> propertyEditorCursor) pp
        , case propertyEditorCurrentEditor of
              Nothing -> emptyWidget
              Just e -> renderEditor (txt . T.concat) True e
        ]
  where
    go :: Maybe [Int] -> PersonProperty -> Widget n
    go msel (PVal PersonPropertyValue {..}) =
        withSelectedAttr msel $ txt personPropertyValueContents
    go msel (PList vs) =
        withSelectedAttr msel $
        vBox $
        map (txt "- " <+>) $
        flip map (zip [0 ..] vs) $ \(ix, v) ->
            let msel' = drillSel msel ix
            in go msel' v
    go msel (PMap tups) =
        withSelectedAttr msel $
        vBox $
        flip map (zip [0 ..] tups) $ \(ix, (k, v)) ->
            let thisSel = drillSel msel ix
                withKeyValueAttr = withSelectedAttr thisSel
                keySel = drillSel thisSel 0
                withKeyAttr = withSelectedAttr keySel
                valueSel = drillSel thisSel 1
                withValueAttr = withSelectedAttr valueSel
                keySide = withKeyAttr $ txt k
                mid = txt ": "
                leftSide = keySide <+> mid
                valueSide = withValueAttr $ go valueSel v
            in withKeyValueAttr $
               case v of
                   (PVal _) -> leftSide <+> valueSide
                   _ -> leftSide <=> padLeft (Pad 2) valueSide
    drillSel msel ix =
        case msel of
            Nothing -> Nothing
            Just [] -> Nothing
            Just (x:xs) ->
                if x == ix
                    then Just xs
                    else Nothing
    withSelectedAttr msel =
        case msel of
            Nothing -> id
            Just [] -> withAttr propertyEditorAttrSelected
            Just _ -> id

propertyEditorAttr :: AttrName
propertyEditorAttr = "property-editor"

propertyEditorAttrSelected :: AttrName
propertyEditorAttrSelected = propertyEditorAttr <> "property-editor-selected"

emptyProperty :: PersonProperty
emptyProperty = PMap []

handlePropertyEditorEvent ::
       (Monoid n, Eq n)
    => Vty.Event
    -> PropertyEditor n
    -> EventM n (PropertyEditor n)
handlePropertyEditorEvent e pe@PropertyEditor {..} =
    case rebuild <$> propertyEditorCursor of
        Nothing ->
            case e of
                (EvKey (V.KChar 's') []) ->
                    pure pe {propertyEditorCursor = Just $ cursor emptyProperty}
                _ -> pure pe
        Just prop ->
            case propertyEditorCurrentEditor of
                Nothing ->
                    case e of
                        (EvKey KDown []) -> moveDown pe prop
                        (EvKey KUp []) -> moveUp pe prop
                        (EvKey KLeft []) -> moveLeft pe prop
                        (EvKey KRight []) -> moveRight pe prop
                        (EvKey KEnter []) -> tryToStartSubEditor pe
                        (EvKey (KChar 'e') []) -> tryToStartSubEditor pe
                        _ -> pure pe
                Just ed ->
                    case e of
                        (EvKey KEnter []) ->
                            pure $ pe {propertyEditorCurrentEditor = Nothing}
                        _ -> do
                            ne <- handleEditorEvent e ed
                            pure $ pe {propertyEditorCurrentEditor = Just ne}

tryToStartSubEditor ::
       Monoid n => PropertyEditor n -> EventM n (PropertyEditor n)
tryToStartSubEditor pe@PropertyEditor {..} =
    case propertyEditorCursor of
        Nothing -> pure pe
        Just cur ->
            let mContents =
                    case cur of
                        APropC (ValC vc) ->
                            Just $
                            personPropertyValueContents $ valCursorSelected vc
                        AKC kc -> Just $ keyCursorSelected kc
                        _ -> Nothing
            in case mContents of
                   Nothing -> pure pe
                   Just cts ->
                       pure $
                       pe
                       { propertyEditorCurrentEditor =
                             Just $
                             editorText
                                 (propertyEditorName <>
                                  propertyEditorName -- Weird hack to get the name to be unique.
                                  )
                                 (Just 1)
                                 cts
                       }

tryToQuitAndSaveEditor :: PropertyEditor n -> EventM n (PropertyEditor n)
tryToQuitAndSaveEditor pe@PropertyEditor {..} =
    case propertyEditorCurrentEditor of
        Nothing -> pure pe
        Just ed ->
            case propertyEditorCursor of
                Nothing -> pure pe
                Just cur ->
                    case cur of
                        APropC (ValC vc) -> do
                            now <- liftIO getCurrentTime
                            let contents = T.concat $ getEditContents ed
                            let newValue =
                                    PersonPropertyValue
                                    { personPropertyValueLastUpdatedTimestamp =
                                          now
                                    , personPropertyValueContents = contents
                                    }
                            pure $
                                pe
                                { propertyEditorCursor =
                                      Just $
                                      APropC $
                                      ValC $
                                      valCursorModifyValue (const newValue) vc
                                }
                        _ -> pure pe

data Selection
    = SelectVal PersonProperty
    | SelectListEl Int
                   PersonProperty
    | SelectList [PersonProperty]
    | SelectKey Text
    | SelectKeyVal Text
                   PersonProperty
    | SelectMap [(Text, PersonProperty)]
    deriving (Show, Eq, Generic)

-- Try to select a subtree of a 'PersonProperty'
--
-- Returns 'Nothing' if the selection is invalid and 'Just' with the selection
-- if the selection is valid.
select :: Maybe [Int] -> PersonProperty -> Maybe Selection
select Nothing _ = Nothing
select (Just []) p = Just $ SelectVal p
select (Just _) (PVal _) = Nothing
select (Just (i:is)) (PList ls) =
    case ls `atMay` i of
        Nothing -> Nothing
        Just p -> select (Just is) p
select (Just [i]) (PMap ls) =
    case ls `atMay` i of
        Nothing -> Nothing
        Just (k, v) -> Just $ SelectKeyVal k v
select (Just [i, j]) (PMap ls) =
    case ls `atMay` i of
        Nothing -> Nothing
        Just (k, p) ->
            case j of
                0 -> Just $ SelectKey k
                1 -> Just $ SelectVal p
                _ -> Nothing
select (Just (i:1:is)) (PMap ls) =
    case ls `atMay` i of
        Nothing -> Nothing
        Just (_, p) -> select (Just is) p
select _ _ = Nothing

unsnocMay :: [a] -> Maybe ([a], a)
unsnocMay as = (,) <$> initMay as <*> lastMay as

makeNewVerSel ::
       (([Int], Int) -> [Int]) -> Maybe [Int] -> PersonProperty -> Maybe [Int]
makeNewVerSel func msel prop =
    case msel of
        Nothing -> Just []
        Just sel ->
            let newSel = func <$> unsnocMay sel
            in case select newSel prop of
                   Nothing -> msel
                   Just _ -> newSel

makeNewHorSel ::
       Maybe [Int]
    -> ([Int] -> Maybe [Int])
    -> Maybe [Int]
    -> PersonProperty
    -> Maybe [Int]
makeNewHorSel start func msel prop =
    case msel of
        Nothing -> start
        Just sel ->
            let newSel = func sel
            in case select newSel prop of
                   Nothing -> msel
                   Just _ -> newSel

-- modSel ::
--        (Maybe [Int] -> PersonProperty -> Maybe [Int])
--     -> PropertyEditor n
--     -> PersonProperty
--     -> EventM n (PropertyEditor n)
-- modSel func pe prop =
--     pure pe {propertyEditorSelection = func (propertyEditorSelection pe) prop}
moveUp :: PropertyEditor n -> PersonProperty -> EventM n (PropertyEditor n)
moveUp = undefined -- modSel selectionUp

-- selectionUp :: Maybe [Int] -> PersonProperty -> Maybe [Int]
-- selectionUp = makeNewVerSel $ \(is, i) -> is ++ [i - 1]
moveDown :: PropertyEditor n -> PersonProperty -> EventM n (PropertyEditor n)
moveDown = undefined -- modSel selectionDown

-- selectionDown :: Maybe [Int] -> PersonProperty -> Maybe [Int]
-- selectionDown = makeNewVerSel $ \(is, i) -> is ++ [i + 1]
moveLeft :: PropertyEditor n -> PersonProperty -> EventM n (PropertyEditor n)
moveLeft = undefined -- modSel selectionLeft

-- selectionLeft :: Maybe [Int] -> PersonProperty -> Maybe [Int]
-- selectionLeft = makeNewHorSel Nothing initMay
moveRight :: PropertyEditor n -> PersonProperty -> EventM n (PropertyEditor n)
moveRight pe prop = cursorRight pe
    -- modSel selectionRight pe prop

-- selectionRight :: Maybe [Int] -> PersonProperty -> Maybe [Int]
-- selectionRight = makeNewHorSel (Just [0]) $ \is -> Just $ is ++ [0]
cursorRight pe@PropertyEditor {..} = do
    let setCursor c = pure pe {propertyEditorCursor = Just c}
    case propertyEditorCursor of
        Nothing -> pure pe
        Just cur ->
            case cur of
                APropC pc ->
                    case pc of
                        ValC vc -> pure pe -- Can't go right if we're looking at a value.
                        ListC lc ->
                            case listCursorElems lc of
                                [] -> pure pe -- Can't go into a list without elements.
                                (lec:_) -> setCursor $ ALElC lec -- The first list element.
                        MapC mc ->
                            case mapCursorElems mc of
                                [] -> pure pe -- Can't go into a map without elements.
                                (kvc:_) -> setCursor $ AMKVC kvc -- The first map element.
                ALElC lec -> setCursor $ APropC $ listElCursorValue lec
                AKC kc -> pure pe
                AMKVC kvc -> setCursor $ AKC $ keyValCursorKey kvc

