{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SetHo.LookupTree
       ( GraphInfo(..)
       , ListViewInfo(..)
       , newLookupTreeview
       , makeOptionsWidget
       ) where

import qualified Control.Concurrent as CC
import Data.List ( foldl' )
import qualified Data.IORef as IORef
import qualified Data.Tree as Tree
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import Text.Read ( readMaybe )
import qualified Data.Text as T
import Text.Printf ( printf )

import Accessors ( Lookup, AccessorTree(..), Getter(..), Setter(..), accessors )

data ListViewInfo a =
  ListViewInfo
  { lviName :: String
  , lviType :: String
  , lviGetter :: Maybe (Getter a, Setter a)
  , lviMarked :: Bool
  , lviMutator :: a -> a
  , lviUpstreamValue :: a
  , lviShownValue :: String
  }

instance Show a => Show (ListViewInfo a) where
  show (ListViewInfo n t _ _ mut val m) = "ListViewInfo " ++ show (n,t,m,mut val)

-- what the graph should draw
data GraphInfo a =
  GraphInfo { giXScaling :: Bool
            , giXRange :: Maybe (Double,Double)
            , giValue :: a
            }

type SignalTree a = Tree.Forest (String, String, Maybe (Getter a, Setter a))

toSignalTree :: forall a . Lookup a => a -> SignalTree a -- Tree.Forest (String, String, Maybe (Getter a, Setter a))
toSignalTree x = case accessors x of
  (ATGetter _) -> error "makeSignalTree: got an accessor right away"
  d -> Tree.subForest $ head $ makeSignalTree' "" "" d
  where
    makeSignalTree' :: String -> String -> AccessorTree a -> SignalTree a
    makeSignalTree' myName parentName (Data (pn,_) children) =
      [Tree.Node
       (myName, parentName, Nothing)
       (concatMap (\(getterName,child) -> makeSignalTree' getterName pn child) children)
      ]
    makeSignalTree' myName parentName (ATGetter getSet) =
      [Tree.Node (myName, parentName, Just getSet) []]



newLookupTreeview ::
  forall a
  . Lookup a
  => a
  -> Gtk.ListStore a
  -> IO (Gtk.ScrolledWindow, IO a)
newLookupTreeview initialValue msgStore = do
  let signalTree = toSignalTree initialValue

  treeStore <- Gtk.treeStoreNew []
  treeview <- Gtk.treeViewNewWithModel treeStore

  Gtk.treeViewSetHeadersVisible treeview True
  Gtk.treeViewSetEnableTreeLines treeview True
--  Gtk.treeViewSetGridLines treeview Gtk.TreeViewGridLinesVertical
--  Gtk.treeViewSetGridLines treeview Gtk.TreeViewGridLinesBoth

  -- add some columns
  colName    <- Gtk.treeViewColumnNew
  colType    <- Gtk.treeViewColumnNew
  colUpstreamValue <- Gtk.treeViewColumnNew
  colStagedValue   <- Gtk.treeViewColumnNew
  colBool    <- Gtk.treeViewColumnNew
  colSpin    <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle colName "name"
  Gtk.treeViewColumnSetTitle colBool "bool"
  Gtk.treeViewColumnSetTitle colType "type"
  Gtk.treeViewColumnSetTitle colUpstreamValue "upstream"
  Gtk.treeViewColumnSetTitle colStagedValue "staged"
  Gtk.treeViewColumnSetTitle colSpin "spin"

  rendererName <- Gtk.cellRendererTextNew
  rendererBool <- Gtk.cellRendererToggleNew
  rendererType <- Gtk.cellRendererTextNew
  rendererStagedValue <- Gtk.cellRendererTextNew
  rendererUpstreamValue <- Gtk.cellRendererTextNew
  rendererSpin <- Gtk.cellRendererSpinNew

  Gtk.cellLayoutPackStart colName    rendererName True
  Gtk.cellLayoutPackStart colType    rendererType True
  Gtk.cellLayoutPackStart colUpstreamValue rendererUpstreamValue True
  Gtk.cellLayoutPackStart colStagedValue   rendererStagedValue True
  Gtk.cellLayoutPackStart colBool    rendererBool True
  Gtk.cellLayoutPackStart colSpin    rendererSpin True

  _ <- Gtk.treeViewAppendColumn treeview colName
  _ <- Gtk.treeViewAppendColumn treeview colType
  _ <- Gtk.treeViewAppendColumn treeview colUpstreamValue
  _ <- Gtk.treeViewAppendColumn treeview colStagedValue
  _ <- Gtk.treeViewAppendColumn treeview colBool
  _ <- Gtk.treeViewAppendColumn treeview colSpin

  -- data name
  let showName (Just _) name _ = name
      showName Nothing name "" = name
      showName Nothing name typeName = name ++ " (" ++ typeName ++ ")"

  Gtk.cellLayoutSetAttributes colName rendererName treeStore $
    \(ListViewInfo {lviName = name, lviType = typeName, lviGetter = getter}) ->
      [ Gtk.cellText := showName getter name typeName
      ]

  -- data type
  let showType (Just (GetBool _, SetBool _)) = "Bool"
      showType (Just (GetDouble _, SetDouble _)) = "Double"
      showType (Just (GetFloat _, SetFloat _)) = "Float"
      showType (Just (GetInt _, SetInt _)) = "Int"
      showType (Just (GetString _, SetString _)) = "String"
      showType _ = ""
 
  Gtk.cellLayoutSetAttributes colType rendererType treeStore $
        \lvi -> [ Gtk.cellText := showType (lviGetter lvi) ]

  -- upstream value
  let showUpstreamValue lvi = case lviGetter lvi of
         (Just (GetBool get, _)) -> show (get upstream)
         (Just (GetDouble get, _)) -> printf "%.2g" (get upstream)
         (Just (GetFloat get, _))  -> printf "%.2g" (get upstream)
         (Just (GetInt get, _))  -> show (get upstream)
         (Just (GetString get, _))  -> get upstream
         _ -> ""
         where
           upstream = lviUpstreamValue lvi

  Gtk.cellLayoutSetAttributes colUpstreamValue rendererUpstreamValue treeStore $
        \lvi -> [ Gtk.cellText := showUpstreamValue lvi
                , Gtk.cellTextEditable := False
                ]

  -- staged value
  let showStagedValue lvi = case lviGetter lvi of
         Just (GetBool get, _) -> show (get staged)
         Just (GetDouble get, _) -> printf "%.2g" (get staged)
         Just (GetFloat get, _)  -> printf "%.2g" (get staged)
         Just (GetInt get, _)  -> show (get staged)
         Just (GetString get, _)  -> get staged
         _ -> ""
         where
           staged = lviMutator lvi (lviUpstreamValue lvi)

  Gtk.cellLayoutSetAttributes colStagedValue rendererStagedValue treeStore $
        \lvi -> case lviGetter lvi of
           Just _ -> [ Gtk.cellText := showStagedValue lvi
                     , Gtk.cellTextEditable := True
                     ]
           Nothing -> [ Gtk.cellText := ""
                      , Gtk.cellTextEditable := False
                      ]
  _ <- on rendererStagedValue Gtk.edited $ \treePath txt -> do
    let _ = txt :: String
    lvi0 <- Gtk.treeStoreGetValue treeStore treePath
    let lvi = case lviGetter lvi0 of
          Just (_, SetBool set)
            | txt `elem` ["t","true","True","1"] -> lvi0 { lviMutator = set True, lviMarked = True }
            | txt `elem` ["f","false","False","0"] -> lvi0 {lviMutator = set False, lviMarked = False }
            | otherwise -> lvi0
          Just (_, SetDouble set) -> case readMaybe txt of
             Nothing -> lvi0
             Just x -> lvi0 { lviMutator = set x }
          Just (_, SetFloat set) -> case readMaybe txt of
             Nothing -> lvi0
             Just x -> lvi0 { lviMutator = set x }
          Just (_, SetInt set) -> case readMaybe txt of
             Nothing -> lvi0
             Just x -> lvi0 { lviMutator = set x }
          Just (_, SetString set) -> lvi0 { lviMutator = set txt }
          Just (_, SetSorry) -> lvi0
          Nothing -> lvi0
    Gtk.treeStoreSetValue treeStore treePath lvi
    return ()

  -- bool
  let toShownBool marked (Just (GetBool _, SetBool _)) =
         [ Gtk.cellToggleInconsistent := False
         , Gtk.cellToggleActive := marked
         , Gtk.cellToggleActivatable := True
         , Gtk.cellToggleRadio := True
         , Gtk.cellToggleIndicatorSize := 12
         ]
      toShownBool _ _ =
         [ Gtk.cellToggleInconsistent := True
         , Gtk.cellToggleActive := False
         , Gtk.cellToggleActivatable := False
         , Gtk.cellToggleRadio := True
         , Gtk.cellToggleIndicatorSize := 0
         ]

  Gtk.cellLayoutSetAttributes colBool rendererBool treeStore $
        \lvi -> toShownBool (lviMarked lvi) (lviGetter lvi)

  _ <- on rendererBool Gtk.cellToggled $ \pathStr -> do
    let treePath = Gtk.stringToTreePath pathStr
    lvi0 <- Gtk.treeStoreGetValue treeStore treePath
    let newMarked :: Bool
        newMarked = not (lviMarked lvi0)
        newMutator :: a -> a
        newMutator = case lviGetter lvi0 of
          Just (_, SetBool set) -> set newMarked
          _ -> error "the new mutator must be a bool mutator"
    Gtk.treeStoreSetValue treeStore treePath (lvi0 {lviMarked = newMarked, lviMutator = newMutator})
    return ()

  -- spin
  let toSpin _lvi = []
  Gtk.cellLayoutSetAttributes colSpin rendererSpin treeStore toSpin


  let -- build the signal tree
      convert :: Tree.Tree (String, String, Maybe (Getter a, Setter a))
                 -> Tree.Tree (ListViewInfo a)
      convert (Tree.Node (name, typ, getter) others) =
        Tree.Node (ListViewInfo name typ getter marked id initialValue "") (map convert others)
        where
          marked = case getter of
            Just (GetBool get, _) -> get initialValue
            _ -> False

  Gtk.treeStoreClear treeStore
  Gtk.treeStoreInsertForest treeStore [] 0 (map convert signalTree)

  let forEach :: (ListViewInfo a -> IO (ListViewInfo a)) -> IO ()
      forEach f = Gtk.treeModelForeach treeStore $ \treeIter -> do
         treePath <- Gtk.treeModelGetPath treeStore treeIter
         lvi0 <- Gtk.treeStoreGetValue treeStore treePath
         lvi1 <- f lvi0
         Gtk.treeStoreSetValue treeStore treePath lvi1
         return False

  latestUpstreamRef <- IORef.newIORef initialValue
  let gotNewValue val = do
        IORef.writeIORef latestUpstreamRef val
        forEach (\lvi -> return (lvi {lviUpstreamValue = val}))

  -- on insert or change, rebuild the signal tree
  _ <- on msgStore Gtk.rowChanged $ \_ changedPath -> do
    newMsg <- Gtk.listStoreGetValue msgStore (Gtk.listStoreIterToIndex changedPath)
    gotNewValue newMsg

  _ <- on msgStore Gtk.rowInserted $ \_ changedPath -> do
    newMsg <- Gtk.listStoreGetValue msgStore (Gtk.listStoreIterToIndex changedPath)
    gotNewValue newMsg

  scroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.containerAdd scroll treeview
  Gtk.set scroll [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
                 , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
                 ]

  let getAll :: IO [ListViewInfo a]
      getAll = do
         lvisRef <- IORef.newIORef []
         Gtk.treeModelForeach treeStore $ \treeIter -> do
            treePath <- Gtk.treeModelGetPath treeStore treeIter
            lvi <- Gtk.treeStoreGetValue treeStore treePath
            IORef.modifyIORef lvisRef (lvi:)
            return False
         fmap reverse (IORef.readIORef lvisRef)
  let getLatest = do
        lvis <- getAll
        latestUpstream <- IORef.readIORef latestUpstreamRef
        return (foldl' (flip lviMutator) latestUpstream lvis)

  return (scroll, getLatest)



makeOptionsWidget :: CC.MVar (GraphInfo a) -> IO Gtk.VBox
makeOptionsWidget graphInfoMVar = do
  -- user selectable range
  xRange <- Gtk.entryNew
  Gtk.set xRange [ Gtk.entryEditable := False
                 , Gtk.widgetSensitive := False
                 ]
  xRangeBox <- labeledWidget "x range:" xRange
  Gtk.set xRange [Gtk.entryText := "(-10,10)"]
  let updateXRange = do
        Gtk.set xRange [ Gtk.entryEditable := True
                       , Gtk.widgetSensitive := True
                       ]
        txt <- Gtk.get xRange Gtk.entryText
        gi <- CC.readMVar graphInfoMVar
        case readMaybe txt of
          Nothing -> do
            putStrLn $ "invalid x range entry: " ++ txt
            Gtk.set xRange [Gtk.entryText := "(min,max)"]
          Just (z0,z1) -> if z0 >= z1
                    then do
                      putStrLn $ "invalid x range entry (min >= max): " ++ txt
                      Gtk.set xRange [Gtk.entryText := "(min,max)"]
                      return ()
                    else do
                      _ <- CC.swapMVar graphInfoMVar (gi {giXRange = Just (z0,z1)})
                      return ()
  _ <- on xRange Gtk.entryActivate updateXRange

  -- linear or log scaling on the x and y axis?
  xScalingSelector <- Gtk.comboBoxNewText
  mapM_ (Gtk.comboBoxAppendText xScalingSelector . T.pack)
    ["linear (auto)","linear (manual)","logarithmic (auto)"]
  Gtk.comboBoxSetActive xScalingSelector 0
  xScalingBox <- labeledWidget "x scaling:" xScalingSelector
  let updateXScaling = do
        k <- Gtk.comboBoxGetActive xScalingSelector
        case k of
          0 -> do
            Gtk.set xRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = False, giXRange = Nothing}
          1 -> do
            Gtk.set xRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = True, giXRange = Nothing}
          _ -> error "the \"impossible\" happened: x scaling comboBox index should be < 3"
  updateXScaling
  _ <- on xScalingSelector Gtk.changed updateXScaling

  -- vbox to hold the little window on the left
  vbox <- Gtk.vBoxNew False 4

  Gtk.set vbox [ Gtk.containerChild := xScalingBox
               , Gtk.boxChildPacking   xScalingBox := Gtk.PackNatural
               , Gtk.containerChild := xRangeBox
               , Gtk.boxChildPacking   xRangeBox := Gtk.PackNatural
               ]

  return vbox



-- helper to make an hbox with a label
labeledWidget :: Gtk.WidgetClass a => String -> a -> IO Gtk.HBox
labeledWidget name widget = do
  label <- Gtk.labelNew (Just name)
  hbox <- Gtk.hBoxNew False 4
  Gtk.set hbox [ Gtk.containerChild := label
               , Gtk.containerChild := widget
               , Gtk.boxChildPacking label := Gtk.PackNatural
--               , Gtk.boxChildPacking widget := Gtk.PackNatural
               ]
  return hbox
