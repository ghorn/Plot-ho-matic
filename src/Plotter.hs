{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module Plotter
       ( Lookup
       , Channel
       , SignalTree
       , AccessorTree(..)
       , newChannel
       , makeSignalTree
       , runPlotter
       , accessors
       ) where

import qualified Data.Sequence as S
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import Data.Time ( getCurrentTime, diffUTCTime )
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
--import System.IO ( withFile, IOMode ( WriteMode ) )
--import qualified Data.ByteString.Lazy as BSL
import qualified Data.Tree as Tree

import qualified GHC.Stats

import PlotTypes --( Channel(..), SignalTree )
import Accessors
import GraphWidget ( newGraph )

makeSignalTree :: Lookup a => a -> SignalTree a
makeSignalTree x = case accessors x of
  (ATGetter _) -> error "makeSignalTree: got an accessor right away"
  d -> Tree.subForest $ head $ makeSignalTree' "" "" d
  where
    makeSignalTree' :: String -> String -> AccessorTree a -> SignalTree a
    makeSignalTree' myName parentName (Data (pn,_) children) =
      [Tree.Node
       (myName, parentName, Nothing)
       (concatMap (\(getterName,child) -> makeSignalTree' getterName pn child) children)
      ]
    makeSignalTree' myName parentName (ATGetter getter) =
      [Tree.Node (myName, parentName, Just (Left getter)) []]


--newChannel :: SignalTree a -> Plotter (a -> IO (), SignalTree a -> IO ())
newChannel :: forall a .
              String -> SignalTree a -> IO (Channel a, a -> IO (), SignalTree a -> IO ())
newChannel name signalTree0 = do
  time0 <- getCurrentTime

  trajChan <- STM.atomically STM.newTQueue
  trajMv <- CC.newMVar S.empty
  maxHistMv <- CC.newMVar 200

  signalTreeStore <- Gtk.listStoreNew []

  let getLastValue :: IO a
      getLastValue = do
        val <- STM.atomically (STM.readTQueue trajChan)
        empty <- STM.atomically (STM.isEmptyTQueue trajChan)
        if empty then return val else getLastValue
  

  let rebuildSignalTree newSignalTree = do
        --putStrLn $ "rebuilding signal tree"
        size <- Gtk.listStoreGetSize signalTreeStore
        if size == 0
          then Gtk.listStorePrepend signalTreeStore newSignalTree
          else Gtk.listStoreSetValue signalTreeStore 0 newSignalTree
              
  -- this is the loop that reads new messages and stores them
  let serverLoop :: Int -> IO ()
      serverLoop k = do
        -- wait until a new message is written to the Chan
        newPoint0 <- getLastValue

        -- grab the timestamp
        time <- getCurrentTime

        -- write to the mvar
        maxHist <- CC.readMVar maxHistMv
        let newPoint = (newPoint0, k, diffUTCTime time time0)
            addPoint lst0
              | S.length lst0 < maxHist = lst0 S.|> newPoint
              | otherwise = case S.viewl lst0 of
                S.EmptyL -> S.singleton newPoint
                _ S.:< lst' -> lst' S.|> newPoint
        _ <- CC.modifyMVar_ trajMv (return . addPoint)
        return ()

        -- loop forever
        serverLoop (k+1)

  rebuildSignalTree signalTree0
  serverTid <- CC.forkIO (serverLoop 0)
  let retChan = Channel { chanName = name
                        , chanTraj = trajMv
                        , chanSignalTreeStore = signalTreeStore
                        , chanServerThreadId = serverTid
                        , chanMaxHist = maxHistMv
                        }

  return (retChan, STM.atomically . STM.writeTQueue trajChan, Gtk.postGUISync . rebuildSignalTree)

runPlotter :: Channel a -> [CC.ThreadId] -> IO ()
runPlotter channel backgroundThreadsToKill = do
  statsEnabled <- GHC.Stats.getGCStatsEnabled
  if statsEnabled
    then do putStrLn "stats enabled"
            stats <- GHC.Stats.getGCStats
            print stats
    else putStrLn "stats not enabled"

  _ <- Gtk.initGUI

  -- start the main window
  win <- Gtk.windowNew
  _ <- Gtk.set win [ Gtk.containerBorderWidth := 8
                   , Gtk.windowTitle := "Plot-ho-matic"
                   ]

  -- on close, kill all the windows and threads
  graphWindowsToBeKilled <- CC.newMVar []
  let killEverything = do
        gws <- CC.readMVar graphWindowsToBeKilled
        mapM_ Gtk.widgetDestroy gws
        mapM_ CC.killThread backgroundThreadsToKill
        CC.killThread (chanServerThreadId channel)
        Gtk.mainQuit
  _ <- Gtk.onDestroy win killEverything

  --------------- main widget -----------------
  -- button to clear history
  buttonClear <- Gtk.buttonNewWithLabel "clear history"
  _ <- Gtk.onClicked buttonClear $ do
    --let clearChan (Channel {chanSeq=cs}) = void (CC.swapMVar cs Seq.empty)
    let clearChan _ = putStrLn "yeah, history clear doesn't really exist lol"
    clearChan channel

  -- list of channels
  chanWidget <- newChannelWidget channel graphWindowsToBeKilled

  -- vbox to hold buttons
  vbox <- Gtk.vBoxNew False 4
  Gtk.set vbox [ Gtk.containerChild := buttonClear
               , Gtk.containerChild := chanWidget
               ]

  -- add widget to window and show
  _ <- Gtk.set win [ Gtk.containerChild := vbox ]
  Gtk.widgetShowAll win
  Gtk.mainGUI


-- the list of channels
newChannelWidget :: Channel a -> CC.MVar [Gtk.Window] -> IO Gtk.TreeView
newChannelWidget channel graphWindowsToBeKilled = do
  -- create a new tree model
  model <- Gtk.listStoreNew [channel]
  treeview <- Gtk.treeViewNewWithModel model
  Gtk.treeViewSetHeadersVisible treeview True

  -- add some columns
  col0 <- Gtk.treeViewColumnNew
  col1 <- Gtk.treeViewColumnNew
  col2 <- Gtk.treeViewColumnNew
  col3 <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle col0 "channel"
  Gtk.treeViewColumnSetTitle col1 "history"
  Gtk.treeViewColumnSetTitle col2 "new"
  Gtk.treeViewColumnSetTitle col3 "save"

  renderer0 <- Gtk.cellRendererTextNew
  renderer1 <- Gtk.cellRendererTextNew
  renderer2 <- Gtk.cellRendererToggleNew
  renderer3 <- Gtk.cellRendererToggleNew

  Gtk.cellLayoutPackStart col0 renderer0 True
  Gtk.cellLayoutPackStart col1 renderer1 True
  Gtk.cellLayoutPackStart col2 renderer2 True
  Gtk.cellLayoutPackStart col3 renderer3 True

  Gtk.cellLayoutSetAttributes col0 renderer0 model $ \lv -> [ Gtk.cellText := chanName lv]
  Gtk.cellLayoutSetAttributes col2 renderer2 model $ const [ Gtk.cellToggleActive := False]
  Gtk.cellLayoutSetAttributes col3 renderer3 model $ const [ Gtk.cellToggleActive := False]


  _ <- Gtk.treeViewAppendColumn treeview col0
  _ <- Gtk.treeViewAppendColumn treeview col1
  _ <- Gtk.treeViewAppendColumn treeview col2
  _ <- Gtk.treeViewAppendColumn treeview col3

  -- spawn a new graph when a checkbox is clicked
  _ <- on renderer2 Gtk.cellToggled $ \pathStr -> do
    let (i:_) = Gtk.stringToTreePath pathStr
    lv <- Gtk.listStoreGetValue model i
    graphWin <- newGraph (chanName lv) (chanSignalTreeStore lv) (chanTraj lv)

    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))


--  -- save all channel data when this button is pressed
--  _ <- on renderer3 Gtk.cellToggled $ \pathStr -> do
--    let (i:_) = Gtk.stringToTreePath pathStr
--    lv <- Gtk.listStoreGetValue model i
--    let writerThread = do
--          bct <- chanGetByteStrings (lvChan lv)
--          let filename = chanName (lvChan lv) ++ "_log.dat"
--              blah _      sizes [] = return (reverse sizes)
--              blah handle sizes ((x,_,_):xs) = do
--                BSL.hPut handle x
--                blah handle (BSL.length x : sizes) xs
--          putStrLn $ "trying to write file \"" ++ filename ++ "\"..."
--          sizes <- withFile filename WriteMode $ \handle -> blah handle [] bct
--          putStrLn $ "finished writing file, wrote " ++ show (length sizes) ++ " protos"
--
--          putStrLn "writing file with sizes..."
--          writeFile (filename ++ ".sizes") (unlines $ map show sizes)
--          putStrLn "done"
--    _ <- CC.forkIO writerThread
    return ()

  return treeview
