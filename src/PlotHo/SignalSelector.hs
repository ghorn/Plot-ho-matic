{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module PlotHo.SignalSelector
       ( SignalSelector(..)
       , newSignalSelectorArea
       ) where

import qualified Control.Concurrent as CC
import Control.Monad ( void, when )
import Data.Either ( isRight )
import Data.List ( foldl', intercalate )
import qualified Data.Map as M
import Data.Maybe ( isNothing, fromJust )
import qualified Data.Tree as Tree
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )

import PlotHo.PlotTypes
       ( GraphInfo(..), ListViewInfo(..)
       , MarkedState(..), SignalTree )

data SignalSelector a
  = SignalSelector
    { ssTreeView :: Gtk.TreeView
    , ssRebuildSignalTree :: SignalTree a -> IO ()
    , ssReadGraphInfo :: IO (GraphInfo a)
    }

newSignalSelectorArea ::
  forall a . IO () -> IO (SignalSelector a)
newSignalSelectorArea redraw = do
  -- mvar with all the user input
  graphInfoMVar <-
    CC.newMVar
    GraphInfo
    { giGetters = []
    , giTitle = Nothing
    } :: IO (CC.MVar (GraphInfo a))

  treeStore <- Gtk.treeStoreNew []
  treeview <- Gtk.treeViewNewWithModel treeStore

  Gtk.treeViewSetHeadersVisible treeview True

  -- add some columns
  colSignal <- Gtk.treeViewColumnNew
  colVisible <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle colSignal "signal"
  Gtk.treeViewColumnSetTitle colVisible "visible?"

  rendererSignal <- Gtk.cellRendererTextNew
  rendererVisible <- Gtk.cellRendererToggleNew

  Gtk.treeViewColumnPackStart colSignal rendererSignal True
  Gtk.treeViewColumnPackStart colVisible rendererVisible True

  let showName :: Either String (a -> [[(Double, Double)]]) -> [String] -> String
      -- show a getter name
      showName (Right _) (name:_) = name
      showName (Right _) [] = error "showName on field got an empty list"
      -- show a parent without type info
      showName (Left "") (name:_) = name
      -- show a parent with type info
      showName (Left typeName) (name:_) = name ++ " (" ++ typeName ++ ")"
      showName (Left _) [] = error "showName on parent got an empty list"

  Gtk.cellLayoutSetAttributes colSignal rendererSignal treeStore $
    \(ListViewInfo {lviName = name, lviTypeOrGetter = typeOrGetter}) ->
      [ Gtk.cellText := showName typeOrGetter (reverse name)
      ]
  Gtk.cellLayoutSetAttributes colVisible rendererVisible treeStore $ \lvi -> case lviMarked lvi of
    On -> [ Gtk.cellToggleInconsistent := False
          , Gtk.cellToggleActive := True
          ]
    Off -> [ Gtk.cellToggleInconsistent := False
           , Gtk.cellToggleActive := False
           ]
    Inconsistent -> [ Gtk.cellToggleActive := False
                    , Gtk.cellToggleInconsistent := True
                    ]

  void $ Gtk.treeViewAppendColumn treeview colSignal
  void $ Gtk.treeViewAppendColumn treeview colVisible

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

        void $ CC.swapMVar graphInfoMVar (GraphInfo {giGetters = newGetters, giTitle = newTitle})

      i2p i = Gtk.treeModelGetPath treeStore i
      p2i p = do
        mi <- Gtk.treeModelGetIter treeStore p
        case mi of Nothing -> error "no iter at that path"
                   Just i -> return i

  -- update which y axes are visible
  _ <- on rendererVisible Gtk.cellToggled $ \pathStr -> do
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
      rebuildSignalTree :: [Tree.Tree ([String], Either String (a -> [[(Double, Double)]]))]
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

  return
    SignalSelector
    { ssTreeView = treeview
    , ssRebuildSignalTree = rebuildSignalTree
    , ssReadGraphInfo = CC.readMVar graphInfoMVar
    }



-- The greatest common prefix will be the title.
-- Everything after that is the field name.
gettersAndTitle :: forall a . [([String], a)] -> ([(String, a)], Maybe String)
gettersAndTitle fullGetters =
  ( map (\(x,y) -> (intercalate "." x, y)) gettersWithPrefixRemoved
  , mtitle
  )
  where
    mtitle :: Maybe String
    mtitle = case titleNames of
      [] -> Nothing
      ts -> Just $ intercalate "." (reverse ts)

    titleNames :: [String]
    gettersWithPrefixRemoved :: [([String], a)]
    (titleNames, gettersWithPrefixRemoved) = splitPartialCommonPrefix $ splitCommonPrefixes [] fullGetters

    -- split out the first element if there is one
    mhead :: ([String], a) -> Maybe (String, ([String], a))
    mhead (x:xs, y) = Just (x, (xs, y))
    mhead ([], _) = Nothing

    splitCommonPrefixes :: [String] -> [([String], a)] -> ([String], [([String], a)])
    splitCommonPrefixes titles getters0
      | any isNothing mheads = (titles, getters0)
      | otherwise = case heads of
          [] -> (titles, getters0)
          (prefix, _):others
            -- if all prefixes match, do another recursion
            | all ((prefix ==) . fst) others -> splitCommonPrefixes (prefix:titles) (map snd heads)
            -- otherwise we're done
            | otherwise -> (titles, getters0)
      where
        mheads :: [Maybe (String, ([String], a))]
        mheads = map mhead getters0

        heads :: [(String, ([String], a))]
        heads = map fromJust mheads



-- We've already split out all the common whole strings.
-- Now we want to get any partial strings.
splitPartialCommonPrefix :: ([String], [([String], a)]) -> ([String], [([String], a)])
splitPartialCommonPrefix (wholePrefixes, getters)
  -- if there is no common prefix, do nothing
  | null prefix = (wholePrefixes, getters)
  -- If there is a common prefix, add it to the wholePrefixes and remove it from the next names.
  | otherwise = (wholePrefixes ++ [prefix], map (\(x,y) -> (removePrefix x, y)) getters)
  where
    removePrefix :: [String] -> [String]
    removePrefix [] = [] -- No names, I guess don't return anything. I think this is impossible
    removePrefix (x:xs) = case drop (length prefix) x of
      -- If the common prefix is a whole variable name, i guess we shouldn't remove it.
      [] -> x:xs
      -- Normal path
      r -> r:xs

    prefix :: String
    prefix
      | any null names = []
      | otherwise = case map head names of
          -- only do it if there are at least two
          first:others@(_:_) -> foldl' commonPrefix first others
          _ -> []
      where
        names :: [[String]]
        names = map fst getters

    commonPrefix (x:xs) (y:ys)
      | x == y = x : commonPrefix xs ys
    commonPrefix _ _ = []
