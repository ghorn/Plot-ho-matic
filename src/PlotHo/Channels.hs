{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.Channels
       ( Meta
       , newChannel
         -- * internal
       , newChannelWidget
       ) where

import Control.Monad ( void )
import qualified Control.Concurrent as CC
import qualified Data.IORef as IORef
import Data.Tree ( Tree )
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import Text.Read ( readMaybe )
import System.Glib.Signals ( on )
--import System.IO ( withFile, IOMode ( WriteMode ) )
--import qualified Data.ByteString.Lazy as BSL

import PlotHo.GraphWidget ( newGraph )
import PlotHo.PlotTypes ( Channel(..), PlotterOptions(..) )

-- hardcode options for now
plotterOptions :: PlotterOptions
plotterOptions =
  PlotterOptions
  { maxDrawRate = 40
  }


type Meta = [Tree ([String], Either String Int)]

-- | This is the general interface to plot whatever you want.
-- Use this when you want to give the whole time series in one go, rather than one at a time
-- such as with 'addHistoryChannel'.
-- Using types or data, you must encode the signal tree with the message so that
-- the plotter can build you the nice signal tree.
newChannel ::
  forall a
  . String -- ^ channel name
  -> (a -> a -> Bool) -- ^ Is the signal tree the same? This is used for instance if signals have changed and the plotter needs to rebuild the signal tree. This lets you keep the plotter running and change other programs which send messages to the plotter.
  -> (a -> [Tree ([String], Either String (a -> [[(Double, Double)]]))]) -- ^ how to build the signal tree
  -> IO (Channel, a -> IO ()) -- ^ Return a channel and a "new message" function. You should for a thread which receives messages and calls this action.
newChannel name sameSignalTree toSignalTree = do
  msgStore <- Gtk.listStoreNew []
  maxHist <- IORef.newIORef 0

  let newMessage :: a -> IO ()
      newMessage next = do
        -- grab the time and counter
        Gtk.postGUIAsync $ do
          size <- Gtk.listStoreGetSize msgStore
          if size == 0
            then Gtk.listStorePrepend msgStore next
            else Gtk.listStoreSetValue msgStore 0 next

  let retChan = Channel { chanName = name
                        , chanMsgStore = msgStore
                        , chanSameSignalTree = sameSignalTree
                        , chanToSignalTree = toSignalTree
                        , chanMaxHistory = maxHist
                        }

  return (retChan, newMessage)


-- the list of channels
newChannelWidget :: Channel -> CC.MVar [Gtk.Window] -> IO Gtk.VBox
newChannelWidget channel graphWindowsToBeKilled = do
  vbox <- Gtk.vBoxNew False 4

  nameBox' <- Gtk.hBoxNew False 4
  nameBox <- labeledWidget (chanName channel) nameBox'

  buttonsBox <- Gtk.hBoxNew False 4

  -- button to clear history
  buttonAlsoDoNothing <- Gtk.buttonNewWithLabel "also do nothing"
--  void $ Gtk.onClicked buttonAlsoDoNothing $ do
--    putStrLn "i promise, nothing happens"
--    -- CC.modifyMVar_ logData (const (return S.empty))
--    return ()
  let triggerYo action = on buttonAlsoDoNothing Gtk.buttonActivated action >> return ()

  -- button to make a new graph
  buttonNew <- Gtk.buttonNewWithLabel "new graph"
  void $ on buttonNew Gtk.buttonActivated $ do
    graphWin <- newGraph plotterOptions triggerYo channel

    -- add this window to the list to be killed on exit
    CC.modifyMVar_ graphWindowsToBeKilled (return . (graphWin:))


  -- entry to set history length
  maxHistoryEntryAndLabel <- Gtk.hBoxNew False 4
  maxHistoryLabel <- Gtk.vBoxNew False 4 >>= labeledWidget "max history:"
  maxHistoryEntry <- Gtk.entryNew
  Gtk.set maxHistoryEntry
    [ Gtk.entryEditable := True
    , Gtk.widgetSensitive := True
    ]
  Gtk.entrySetText maxHistoryEntry "500"
  let updateMaxHistory = do
        txt <- Gtk.get maxHistoryEntry Gtk.entryText
        let reset = Gtk.entrySetText maxHistoryEntry "(max)"
        case readMaybe txt :: Maybe Int of
          Nothing ->
            putStrLn ("max history: couldn't make an Int out of \"" ++ show txt ++ "\"") >> reset
          Just 0  -> putStrLn ("max history: must be greater than 0") >> reset
          Just k  -> IORef.writeIORef (chanMaxHistory channel) k

  void $ on maxHistoryEntry Gtk.entryActivate updateMaxHistory
  updateMaxHistory


  Gtk.set maxHistoryEntryAndLabel
    [ Gtk.containerChild := maxHistoryLabel
    , Gtk.boxChildPacking maxHistoryLabel := Gtk.PackNatural
    , Gtk.containerChild := maxHistoryEntry
    , Gtk.boxChildPacking maxHistoryEntry := Gtk.PackNatural
    ]


  -- put all the buttons/entries together
  Gtk.set buttonsBox
    [ Gtk.containerChild := buttonNew
    , Gtk.boxChildPacking buttonNew := Gtk.PackNatural
    , Gtk.containerChild := buttonAlsoDoNothing
    , Gtk.boxChildPacking buttonAlsoDoNothing := Gtk.PackNatural
    , Gtk.containerChild := maxHistoryEntryAndLabel
    , Gtk.boxChildPacking maxHistoryEntryAndLabel := Gtk.PackNatural
    ]

  Gtk.set vbox
    [ Gtk.containerChild := nameBox
    , Gtk.boxChildPacking   nameBox := Gtk.PackNatural
    , Gtk.containerChild := buttonsBox
    , Gtk.boxChildPacking   buttonsBox := Gtk.PackNatural
    ]

  return vbox


----  -- save all channel data when this button is pressed
----  void $ on renderer3 Gtk.cellToggled $ \pathStr -> do
----    let (i:_) = Gtk.stringToTreePath pathStr
----    lv <- Gtk.listStoreGetValue model i
----    let writerThread = do
----          bct <- chanGetByteStrings (lvChan lv)
----          let filename = chanName (lvChan lv) ++ "_log.dat"
----              blah _      sizes [] = return (reverse sizes)
----              blah handle sizes ((x,_,_):xs) = do
----                BSL.hPut handle x
----                blah handle (BSL.length x : sizes) xs
----          putStrLn $ "trying to write file \"" ++ filename ++ "\"..."
----          sizes <- withFile filename WriteMode $ \handle -> blah handle [] bct
----          putStrLn $ "finished writing file, wrote " ++ show (length sizes) ++ " protos"
----
----          putStrLn "writing file with sizes..."
----          writeFile (filename ++ ".sizes") (unlines $ map show sizes)
----          putStrLn "done"
----    void $ CC.forkIO writerThread
--    return ()
--
--  return treeview


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
