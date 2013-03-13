{-# OPTIONS_GHC -Wall #-}

module Plotter ( newChannel, runPlotter, makeAccessors ) where

import qualified Control.Concurrent as CC
import qualified Data.Foldable as F
import Data.Sequence ( (|>) )
import qualified Data.Sequence as S
import Data.Time ( getCurrentTime, diffUTCTime )
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import System.IO ( withFile, IOMode ( WriteMode ) )
import qualified Text.ProtocolBuffers as PB
import qualified Data.ByteString.Lazy as BSL

import Accessors ( makeAccessors )
import PlotTypes ( Channel(..), PbTree, pbTreeToTree )
import GraphWidget ( newGraph )
import ReadMaybe ( readMaybe )

data ListView = ListView { lvChan :: Channel
                         , lvMaxHist :: Int
                         }

newChannel :: (PB.ReflectDescriptor a, PB.Wire a) => String -> PbTree a -> IO (Channel, CC.Chan a)
newChannel name pbTree = do
  time0 <- getCurrentTime
  
  seqChan <- CC.newChan
  seqMv <- CC.newMVar S.empty
  maxHistMv <- CC.newMVar (10000 :: Int)

  let serverLoop k = do
              -- wait until a new message is written to the Chan
        newMsg <- CC.readChan seqChan
        -- grab the timestamp
        time <- getCurrentTime
        -- append this to the Seq in the MVar, dropping the excess old messages
        maxNum <- CC.readMVar maxHistMv
        let f seq0 = return $ S.drop (S.length seq0 + 1 - maxNum) (seq0 |> (newMsg, k, diffUTCTime time time0))
        CC.modifyMVar_ seqMv f
        -- loop forever
        serverLoop (k+1)
  
  serverTid <- CC.forkIO $ serverLoop 0
  let retChan = Channel { chanName = name
                        , chanGetters = pbTreeToTree name pbTree
                        , chanSeq = seqMv
                        , chanMaxHist = maxHistMv
                        , chanServerThreadId = serverTid
                        , chanGetByteStrings = cgb
                        }
      cgb = do
        s <- CC.readMVar seqMv
        return $ map (\(x,y,z) -> (PB.messagePut x,y,z)) $ F.toList s

  return (retChan, seqChan)

runPlotter :: [Channel] -> [CC.ThreadId] -> IO ()
runPlotter channels backgroundThreadsToKill = do
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
        mapM_ (CC.killThread . chanServerThreadId) channels
        Gtk.mainQuit
  _ <- Gtk.onDestroy win killEverything

  --------------- main widget -----------------
  -- button to clear history
  buttonClear <- Gtk.buttonNewWithLabel "clear history"
  _ <- Gtk.onClicked buttonClear $ do
    let clearChan (Channel {chanSeq=cs}) = CC.swapMVar cs S.empty >> return ()
    mapM_ clearChan channels

  -- list of channels
  chanWidget <- newChannelWidget channels graphWindowsToBeKilled

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
newChannelWidget :: [Channel] -> CC.MVar [Gtk.Window] -> IO Gtk.TreeView
newChannelWidget channels graphWindowsToBeKilled = do
  -- create a new tree model
  let toListView ch = do
        k <- CC.readMVar $ chanMaxHist ch
        return ListView { lvChan = ch, lvMaxHist = k }
  model <- mapM toListView channels >>= Gtk.listStoreNew
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

  Gtk.cellLayoutSetAttributes col0 renderer0 model $ \lv -> [ Gtk.cellText := chanName (lvChan lv)]
  Gtk.cellLayoutSetAttributes col1 renderer1 model $ \lv -> [ Gtk.cellText := show (lvMaxHist lv)
                                                            , Gtk.cellTextEditable := True
                                                            ]
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
    graphWin <- newGraph (lvChan lv)
    
    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))


  -- save all channel data when this button is pressed
  _ <- on renderer3 Gtk.cellToggled $ \pathStr -> do
    let (i:_) = Gtk.stringToTreePath pathStr
    lv <- Gtk.listStoreGetValue model i
    let writerThread = do
          bct <- chanGetByteStrings (lvChan lv)
          let filename = (chanName (lvChan lv)) ++ "_log.dat"
              blah _      sizes [] = return (reverse sizes)
              blah handle sizes ((x,_,_):xs) = do
                BSL.hPut handle x
                blah handle ((BSL.length x):sizes) xs
          putStrLn $ "trying to write file \"" ++ filename ++ "\"..."
          sizes <- withFile filename WriteMode $ \handle -> blah handle [] bct
          putStrLn $ "finished writing file, wrote " ++ show (length sizes) ++ " protos"

          putStrLn "writing file with sizes..."
          writeFile (filename ++ ".sizes") (unlines $ map show sizes)
          putStrLn "done"
    _ <- CC.forkIO writerThread
    return ()


  -- how long to make the history
  _ <- on renderer1 Gtk.edited $ \treePath txt -> do
    let (i:_) = treePath
    lv <- Gtk.listStoreGetValue model i
    putStrLn $ "history len: " ++ txt
    case readMaybe txt of
      Nothing -> do
        putStrLn $ "invalid non-integer range entry: " ++ txt
        k0 <- CC.readMVar $ chanMaxHist (lvChan lv)
        Gtk.listStoreSetValue model i (lv {lvMaxHist = k0})
      Just k -> if k < 0
                then do
                  putStrLn $ "invalid negative range entry: " ++ txt
                  k0 <- CC.readMVar $ chanMaxHist (lvChan lv)
                  Gtk.listStoreSetValue model i (lv {lvMaxHist = k0})
                else do
                  _ <- CC.swapMVar (chanMaxHist (lvChan lv)) k
                  return ()

  return treeview
