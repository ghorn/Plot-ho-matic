{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -ddump-splices #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}

module Main where

import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.ModelView as New

import System.Glib.Signals (on)
import Data.List ( isPrefixOf )
import Data.Char ( toLower )

--import qualified Data.Tree as Tree

import Quotes --( f, MyType(..) )

data Xyz = MkXyz { x_ :: Double
                 , y_ :: Double
                 , z_ :: Double
                 }
data Axyz = MkAxyz { a_ :: Double
                   , xyz_ :: Xyz
                   }

anAxyz :: Axyz
anAxyz = MkAxyz 7 (MkXyz 1 2 3)

increment :: Axyz -> Axyz
increment (MkAxyz a (MkXyz x y z)) = MkAxyz (a+1) (MkXyz (x + 0.1) (y + 0.2) (z + 0.3))

go :: IO ()
go = do
  (receiveNewMessage, infos) <- $(setupTelem "position" ''Axyz)
  putStrLn "yay"

  let printLog = mapM_ printVarInfo infos

      updateLoop 0 _ = return ()
      updateLoop n anAxyz' = do
        receiveNewMessage anAxyz'
        putStrLn ""
        printLog
        updateLoop (n-1::Int) (increment anAxyz')

  printLog
  updateLoop 4 anAxyz


--data Phone = Phone { name :: String, number :: Int, marked :: Bool }

data VarInfo' = VarInfo' { name :: String
                         , pc' :: PContainer
                         , marked :: Bool
                         }

main :: IO ()
main = do
  (_receiveNewMessage, infos) <- $(setupTelem "position" ''Axyz)

  _ <- G.initGUI

  win <- G.windowNew
  _ <- G.onDestroy win G.mainQuit

  -- create a new tree model
  model <- G.listStoreNew $ map (\(VarInfo st pc) -> VarInfo' st pc False) infos
  view <- New.treeViewNewWithModel model

  New.treeViewSetHeadersVisible view True

  -- add three columns
  col1 <- New.treeViewColumnNew
  col2 <- New.treeViewColumnNew
  col3 <- New.treeViewColumnNew

  New.treeViewColumnSetTitle col1 "name"
  New.treeViewColumnSetTitle col2 "Int column"
  New.treeViewColumnSetTitle col3 "show?"

  renderer1 <- New.cellRendererTextNew
  renderer2 <- New.cellRendererTextNew
  renderer3 <- New.cellRendererToggleNew

  New.cellLayoutPackStart col1 renderer1 True
  New.cellLayoutPackStart col2 renderer2 True
  New.cellLayoutPackStart col3 renderer3 True

  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := name row ]
  New.cellLayoutSetAttributes col2 renderer2 model $ \_ -> [ New.cellText := show "waaa" ]
  New.cellLayoutSetAttributes col3 renderer3 model $ \row -> [ New.cellToggleActive := marked row ]

  _ <- New.treeViewAppendColumn view col1
  _ <- New.treeViewAppendColumn view col2
  _ <- New.treeViewAppendColumn view col3

  -- update the model when the toggle buttons are activated
  _ <- on renderer3 G.cellToggled $ \pathStr -> do
    let (i:_) = G.stringToTreePath pathStr
    val <- G.listStoreGetValue model i
    G.listStoreSetValue model i val { marked = not (marked val) }


  -- enable interactive search
  New.treeViewSetEnableSearch view True
  New.treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
    (i:_) <- G.treeModelGetPath model iter
    row <- G.listStoreGetValue model i
    return (map toLower str `isPrefixOf` map toLower (name row))

  G.containerAdd win view
  G.widgetShowAll win
  G.mainGUI
