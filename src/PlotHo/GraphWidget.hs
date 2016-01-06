{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module PlotHo.GraphWidget
       ( newGraph
       ) where

import qualified Control.Concurrent as CC
import Control.Monad ( void, when, unless )
import Data.Either ( isRight )
import qualified Data.IORef as IORef
import Data.List ( intercalate )
import qualified Data.Map as M
import Data.Maybe ( isNothing, fromJust )
import qualified Data.Tree as Tree
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import Text.Read ( readMaybe )
import qualified Data.Text as T
import qualified Graphics.Rendering.Chart as Chart

import PlotHo.PlotChart ( AxisScaling(..), displayChart, chartGtkUpdateCanvas )
import PlotHo.PlotTypes ( GraphInfo(..), ListViewInfo(..), MarkedState(..) )

-- make a new graph window
newGraph ::
  forall a
  . (IO () -> IO ())
  -> String
  -> (a -> a -> Bool)
  -> (a -> [Tree.Tree ([String], Either String (a -> [[(Double, Double)]]))])
  -> Gtk.ListStore a -> IO Gtk.Window
newGraph onButton channame sameSignalTree forestFromMeta msgStore = do
  win <- Gtk.windowNew

  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := channame
                   ]

  -- mvar with all the user input
  graphInfoMVar <- CC.newMVar GraphInfo { giXScaling = LinearScaling
                                        , giYScaling = LinearScaling
                                        , giXRange = Nothing
                                        , giYRange = Nothing
                                        , giGetters = []
                                        , giTitle = Nothing
                                        } :: IO (CC.MVar (GraphInfo a))

  let makeRenderable :: IO (Chart.Renderable ())
      makeRenderable = do
        gi <- CC.readMVar graphInfoMVar
        size <- Gtk.listStoreGetSize msgStore

        namePcs <- if size == 0
                   then return []
                   else do
                     datalog <- Gtk.listStoreGetValue msgStore 0
                     let ret :: [(String, [[(Double,Double)]])]
                         ret = map (fmap (\g -> g datalog)) (giGetters gi)
                     return ret
        return $ displayChart (giXScaling gi, giYScaling gi) (giXRange gi, giYRange gi) (giTitle gi) namePcs

  -- chart drawing area
  chartCanvas <- Gtk.drawingAreaNew
  _ <- Gtk.widgetSetSizeRequest chartCanvas 250 250

  latestRenderableMVar <- CC.newEmptyMVar

  let redraw :: IO ()
      redraw = do
        renderable <- makeRenderable
        maybeLatestRenderable <- CC.tryTakeMVar latestRenderableMVar
        case maybeLatestRenderable of
         -- the other action is still waiting
         Just _ -> CC.putMVar latestRenderableMVar renderable
         -- there is no action waiting, post the action
         Nothing -> do CC.putMVar latestRenderableMVar renderable
                       void $ flip Gtk.idleAdd Gtk.priorityDefaultIdle $ do
                         -- this might not be the same one if the messages have accumulated
                         latestRenderable <- CC.takeMVar latestRenderableMVar
                         chartGtkUpdateCanvas latestRenderable chartCanvas
                         return False -- we're done now, don't call this again

  _ <- Gtk.onExpose chartCanvas $ const (redraw >> return True)


  -- the options widget
  optionsWidget <- makeOptionsWidget graphInfoMVar redraw
  options <- Gtk.expanderNew "options"
  Gtk.set options [ Gtk.containerChild := optionsWidget
                  , Gtk.expanderExpanded := False
                  ]


  -- the signal selector
  treeview' <- newSignalSelectorArea onButton sameSignalTree forestFromMeta graphInfoMVar msgStore redraw
  treeview <- Gtk.expanderNew "signals"
  Gtk.set treeview [ Gtk.containerChild := treeview'
                   , Gtk.expanderExpanded := True
                   ]

  -- options and signal selector packed in vbox
  vboxOptionsAndSignals <- Gtk.vBoxNew False 4
  Gtk.set vboxOptionsAndSignals
    [ Gtk.containerChild := options
    , Gtk.boxChildPacking options := Gtk.PackNatural
    , Gtk.containerChild := treeview
    , Gtk.boxChildPacking treeview := Gtk.PackGrow
    ]

  -- hbox to hold eveything
  hboxEverything <- Gtk.hBoxNew False 4
  Gtk.set hboxEverything
    [ Gtk.containerChild := vboxOptionsAndSignals
    , Gtk.boxChildPacking vboxOptionsAndSignals := Gtk.PackNatural
    , Gtk.containerChild := chartCanvas
    ]
  _ <- Gtk.set win [ Gtk.containerChild := hboxEverything ]

  Gtk.widgetShowAll win
  return win

-- The greatest common prefix will be the title.
-- Everything after that is the field name.
gettersAndTitle :: forall a . [([String], a)] -> ([(String, a)], Maybe String)
gettersAndTitle getters0 = (getters2, titles'')
  where
    titles'' = case titles' of
      [] -> Nothing
      ts -> Just $ intercalate "." (reverse ts)
    titles' :: [String]

    (titles', getters1) = f [] getters0
    getters2 = map (\(x,y) -> (intercalate "." x, y)) getters1

    extractHead (x:xs, y) = Just (x, (xs, y))
    extractHead ([], _) = Nothing

    f titles xs0
      | any isNothing xs = (titles, xs0)
      | otherwise = case xs' of
          [] -> (titles, xs0)
          (prefix, _):others
            -- if all prefixes match, do another recursion
            | all ((prefix ==) . fst) others -> f (prefix:titles) (map snd xs')
            -- otherwise we're done
            | otherwise -> (titles, xs0)
      where
        xs :: [Maybe (String, ([String], a))]
        xs = map extractHead xs0

        xs' :: [(String, ([String], a))]
        xs' = map fromJust xs

newSignalSelectorArea ::
  forall a
  . (IO () -> IO ())
  -> (a -> a -> Bool)
  -> (a -> [Tree.Tree ([String], Either String (a -> [[(Double, Double)]]))])
  -> CC.MVar (GraphInfo a)
  -> Gtk.ListStore a
  -> IO () -> IO Gtk.ScrolledWindow
newSignalSelectorArea onButton sameSignalTree forestFromMeta graphInfoMVar msgStore redraw = do
  treeStore <- Gtk.treeStoreNew []
  treeview <- Gtk.treeViewNewWithModel treeStore

  Gtk.treeViewSetHeadersVisible treeview True

  -- add some columns
  col1 <- Gtk.treeViewColumnNew
  col2 <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle col1 "signal"
  Gtk.treeViewColumnSetTitle col2 "visible?"

  renderer1 <- Gtk.cellRendererTextNew
  renderer2 <- Gtk.cellRendererToggleNew

  Gtk.cellLayoutPackStart col1 renderer1 True
  Gtk.cellLayoutPackStart col2 renderer2 True

  let showName :: Either String (a -> [[(Double, Double)]]) -> [String] -> String
      -- show a getter name
      showName (Right _) (name:_) = name
      showName (Right _) [] = error "showName on field got an empty list"
      -- show a parent without type info
      showName (Left "") (name:_) = name
      -- show a parent with type info
      showName (Left typeName) (name:_) = name ++ " (" ++ typeName ++ ")"
      showName (Left _) [] = error "showName on parent got an empty list"

  Gtk.cellLayoutSetAttributes col1 renderer1 treeStore $
    \(ListViewInfo {lviName = name, lviTypeOrGetter = typeOrGetter}) ->
      [ Gtk.cellText := showName typeOrGetter (reverse name)
      ]
  Gtk.cellLayoutSetAttributes col2 renderer2 treeStore $ \lvi -> case lviMarked lvi of
    On -> [ Gtk.cellToggleInconsistent := False
          , Gtk.cellToggleActive := True
          ]
    Off -> [ Gtk.cellToggleInconsistent := False
           , Gtk.cellToggleActive := False
           ]
    Inconsistent -> [ Gtk.cellToggleActive := False
                    , Gtk.cellToggleInconsistent := True
                    ]

  _ <- Gtk.treeViewAppendColumn treeview col1
  _ <- Gtk.treeViewAppendColumn treeview col2


  let -- update the graph information
      updateGraphInfo = do
        -- first get all trees
        let getTrees k = do
              tree' <- Gtk.treeStoreLookup treeStore [k]
              case tree' of Nothing -> return []
                            Just tree -> fmap (tree:) (getTrees (k+1))
        theTrees <- getTrees 0
        let fromRight (Right r) = r
            fromRight (Left _) =  error "PlotHo GraphWidget: fromRight got Left, this should be impossible"
            newGetters0 :: [([String], a -> [[(Double, Double)]])]
            newGetters0 = [ (lviName lvi, fromRight $ lviTypeOrGetter lvi)
                          | lvi <- concatMap Tree.flatten theTrees
                          , lviMarked lvi == On
                          , isRight (lviTypeOrGetter lvi)
                          ]
        let newGetters :: [(String, a -> [[(Double, Double)]])]
            newTitle :: Maybe String
            (newGetters, newTitle) = gettersAndTitle newGetters0

        _ <- CC.modifyMVar_ graphInfoMVar $
             \gi0 -> return $ gi0 {giGetters = newGetters, giTitle = newTitle}
        return ()

      i2p i = Gtk.treeModelGetPath treeStore i
      p2i p = do
        mi <- Gtk.treeModelGetIter treeStore p
        case mi of Nothing -> error "no iter at that path"
                   Just i -> return i

  -- update which y axes are visible
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    let treePath = Gtk.stringToTreePath pathStr

        getChildrenPaths path' = do
          iter' <- p2i path'
          let getChildPath k = do
                mc <- Gtk.treeModelIterNthChild treeStore (Just iter') k
                case mc of
                  Nothing -> error "no child"
                  Just c -> i2p c
          n <- Gtk.treeModelIterNChildren treeStore (Just iter')
          mapM getChildPath (take n [0..])

        changeSelfAndChildren change path' = do
          childrenPaths <- getChildrenPaths path'
          ret <- Gtk.treeStoreChange treeStore path' change
          when (not ret) $ error "treeStoreChange fail"
          mapM_ (changeSelfAndChildren change) childrenPaths

        fixInconsistent path' = do
          mparentIter <- p2i path' >>= Gtk.treeModelIterParent treeStore
          case mparentIter of
            Nothing -> return ()
            Just parentIter -> do
              parentPath <- i2p parentIter
              siblingPaths <- getChildrenPaths parentPath
              siblings <- mapM (Gtk.treeStoreGetValue treeStore) siblingPaths
              let markedSiblings :: [MarkedState]
                  markedSiblings = map lviMarked siblings

                  changeParent
                    | all (== On) markedSiblings =
                        Gtk.treeStoreChange treeStore parentPath (\lvi -> lvi {lviMarked = On})
                    | all (== Off) markedSiblings =
                        Gtk.treeStoreChange treeStore parentPath (\lvi -> lvi {lviMarked = Off})
                    | otherwise =
                        Gtk.treeStoreChange treeStore parentPath (\lvi -> lvi {lviMarked = Inconsistent})
              ret <- changeParent
              when (not ret) $ error "fixInconsistent couldn't change parent"
              fixInconsistent parentPath
              return ()

    -- toggle the check mark
    val <- Gtk.treeStoreGetValue treeStore treePath
    case val of
      (ListViewInfo _ (Left _) Off) ->
        changeSelfAndChildren (\lvi -> lvi {lviMarked = On}) treePath
      (ListViewInfo _ (Left _) On) ->
        changeSelfAndChildren (\lvi -> lvi {lviMarked = Off}) treePath
      (ListViewInfo _ (Left _) Inconsistent) ->
        changeSelfAndChildren (\lvi -> lvi {lviMarked = On}) treePath
      lvi@(ListViewInfo _ (Right _) On) ->
        Gtk.treeStoreSetValue treeStore treePath $ lvi {lviMarked = Off}
      lvi@(ListViewInfo _ (Right _) Off) ->
        Gtk.treeStoreSetValue treeStore treePath $ lvi {lviMarked = On}
      (ListViewInfo _ (Right _) Inconsistent) -> error "cell getter can't be inconsistent"

    fixInconsistent treePath
    updateGraphInfo
    redraw

  let getTopForest = do
        nTopLevelNodes <- Gtk.treeModelIterNChildren treeStore Nothing
        mnodes <- mapM (Gtk.treeModelIterNthChild treeStore Nothing) (take nTopLevelNodes [0..])
        let treeFromJust (Just x) = i2p x >>= Gtk.treeStoreGetTree treeStore
            treeFromJust Nothing = error "missing top level node"
        mapM treeFromJust mnodes

  -- rebuild the signal tree
  let rebuildSignalTree :: [Tree.Tree ([String], Either String (a -> [[(Double, Double)]]))]
                           -> IO ()
      rebuildSignalTree meta = do
        putStrLn "rebuilding signal tree"

        oldTrees <- getTopForest
        let _ = oldTrees :: [Tree.Tree (ListViewInfo a)]

            merge :: forall b
                     . [Tree.Tree (ListViewInfo b)]
                     -> [Tree.Tree ([String], Either String (a -> [[(Double, Double)]]))]
                     -> [Tree.Tree (ListViewInfo a)]
            merge old new = map convert new
              where
                oldMap :: M.Map ([String], Maybe String) (ListViewInfo b, [Tree.Tree (ListViewInfo b)])
                oldMap = M.fromList $ map f old
                  where
                    f (Tree.Node lvi lvis) = ((lviName lvi, maybeType), (lvi, lvis))
                      where
                        maybeType = case lviTypeOrGetter lvi of
                          Left typ -> Just typ
                          Right _ -> Nothing

                convert :: Tree.Tree ([String], Either String (a -> [[(Double, Double)]]))
                           -> Tree.Tree (ListViewInfo a)
                convert (Tree.Node (name, tog) others) = case M.lookup (name, maybeType) oldMap of
                  Nothing -> Tree.Node (ListViewInfo name tog Off) (merge [] others)
                  Just (lvi, oldOthers) -> Tree.Node (ListViewInfo name tog (lviMarked lvi)) (merge oldOthers others)
                  where
                    maybeType = case tog of
                      Left r -> Just r
                      Right _ -> Nothing

            newTrees = merge oldTrees meta

        Gtk.treeStoreClear treeStore
        Gtk.treeStoreInsertForest treeStore [] 0 newTrees
        updateGraphInfo

  oldMetaRef <- IORef.newIORef Nothing
  let maybeRebuildSignalTree :: a -> IO ()
      maybeRebuildSignalTree newMeta = do
        oldMeta <- IORef.readIORef oldMetaRef
        let sameSignalTree' Nothing _ = False
            sameSignalTree' (Just x) y = sameSignalTree x y
        unless (sameSignalTree' oldMeta newMeta) $ do
          IORef.writeIORef oldMetaRef (Just newMeta)
          rebuildSignalTree (forestFromMeta newMeta)

  -- on insert or change, rebuild the signal tree
  _ <- on msgStore Gtk.rowChanged $ \_ changedPath -> do
    newMsg <- Gtk.listStoreGetValue msgStore (Gtk.listStoreIterToIndex changedPath)
    maybeRebuildSignalTree newMsg >> redraw
  _ <- on msgStore Gtk.rowInserted $ \_ changedPath -> do
    newMsg <- Gtk.listStoreGetValue msgStore (Gtk.listStoreIterToIndex changedPath)
    maybeRebuildSignalTree newMsg >> redraw

  -- rebuild the signal tree right now if it exists
  size <- Gtk.listStoreGetSize msgStore
  when (size > 0) $ do
    newMsg <- Gtk.listStoreGetValue msgStore 0
    maybeRebuildSignalTree newMsg >> redraw

  -- for debugging
  onButton $ do
    newMsg <- Gtk.listStoreGetValue msgStore 0
    rebuildSignalTree (forestFromMeta newMsg) >> redraw


  scroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.containerAdd scroll treeview
  Gtk.set scroll [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
                 , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
                 ]
  return scroll



makeOptionsWidget :: CC.MVar (GraphInfo a) -> IO () -> IO Gtk.VBox
makeOptionsWidget graphInfoMVar redraw = do
  -- user selectable range
  xRange <- Gtk.entryNew
  yRange <- Gtk.entryNew
  Gtk.set xRange [ Gtk.entryEditable := False
                 , Gtk.widgetSensitive := False
                 ]
  Gtk.set yRange [ Gtk.entryEditable := False
                 , Gtk.widgetSensitive := False
                 ]
  xRangeBox <- labeledWidget "x range:" xRange
  yRangeBox <- labeledWidget "y range:" yRange
  Gtk.set xRange [Gtk.entryText := "(-10,10)"]
  Gtk.set yRange [Gtk.entryText := "(-10,10)"]
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
                      redraw
  let updateYRange = do
        Gtk.set yRange [ Gtk.entryEditable := True
                       , Gtk.widgetSensitive := True
                       ]
        txt <- Gtk.get yRange Gtk.entryText
        gi <- CC.readMVar graphInfoMVar
        case readMaybe txt of
          Nothing -> do
            putStrLn $ "invalid y range entry: " ++ txt
            Gtk.set yRange [Gtk.entryText := "(min,max)"]
          Just (z0,z1) -> if z0 >= z1
                    then do
                      putStrLn $ "invalid y range entry (min >= max): " ++ txt
                      Gtk.set yRange [Gtk.entryText := "(min,max)"]
                      return ()
                    else do
                      _ <- CC.swapMVar graphInfoMVar (gi {giYRange = Just (z0,z1)})
                      redraw
  _ <- on xRange Gtk.entryActivate updateXRange
  _ <- on yRange Gtk.entryActivate updateYRange

  -- linear or log scaling on the x and y axis?
  xScalingSelector <- Gtk.comboBoxNewText
  yScalingSelector <- Gtk.comboBoxNewText
  mapM_ (Gtk.comboBoxAppendText xScalingSelector . T.pack)
    ["linear (auto)","linear (manual)","logarithmic (auto)"]
  mapM_ (Gtk.comboBoxAppendText yScalingSelector . T.pack)
    ["linear (auto)","linear (manual)","logarithmic (auto)"]
  Gtk.comboBoxSetActive xScalingSelector 0
  Gtk.comboBoxSetActive yScalingSelector 0
  xScalingBox <- labeledWidget "x scaling:" xScalingSelector
  yScalingBox <- labeledWidget "y scaling:" yScalingSelector
  let updateXScaling = do
        k <- Gtk.comboBoxGetActive xScalingSelector
        _ <- case k of
          0 -> do
            Gtk.set xRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = LinearScaling, giXRange = Nothing}
          1 -> do
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = LinearScaling, giXRange = Nothing}
            updateXRange
          2 -> do
            Gtk.set xRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giXScaling = LogScaling, giXRange = Nothing}
          _ -> error "the \"impossible\" happened: x scaling comboBox index should be < 3"
        redraw
  let updateYScaling = do
        k <- Gtk.comboBoxGetActive yScalingSelector
        _ <- case k of
          0 -> do
            Gtk.set yRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giYScaling = LinearScaling, giYRange = Nothing}
          1 -> do
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giYScaling = LinearScaling, giYRange = Nothing}
            updateYRange
          2 -> do
            Gtk.set yRange [ Gtk.entryEditable := False
                           , Gtk.widgetSensitive := False
                           ]
            CC.modifyMVar_ graphInfoMVar $
              \gi -> return $ gi {giYScaling = LogScaling, giYRange = Nothing}
          _ -> error "the \"impossible\" happened: y scaling comboBox index should be < 3"
        redraw
  updateXScaling
  updateYScaling
  _ <- on xScalingSelector Gtk.changed updateXScaling
  _ <- on yScalingSelector Gtk.changed updateYScaling

  -- vbox to hold the little window on the left
  vbox <- Gtk.vBoxNew False 4

  Gtk.set vbox [ Gtk.containerChild := xScalingBox
               , Gtk.boxChildPacking   xScalingBox := Gtk.PackNatural
               , Gtk.containerChild := xRangeBox
               , Gtk.boxChildPacking   xRangeBox := Gtk.PackNatural
               , Gtk.containerChild := yScalingBox
               , Gtk.boxChildPacking   yScalingBox := Gtk.PackNatural
               , Gtk.containerChild := yRangeBox
               , Gtk.boxChildPacking   yRangeBox := Gtk.PackNatural
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
