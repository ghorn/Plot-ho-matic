{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# Language PackageImports #-}

module SetHo.LookupTree
       ( ListViewElement(..)
       , newLookupTreeview
       ) where

import Accessors.Dynamic
       ( DTree, DData(..), DConstructor(..), DSimpleEnum(..), DField(..)
       , describeDField, sameDFieldType
       , denumToString, denumToStringOrMsg, denumSetString
       , diffDTrees
       )
import Control.Monad ( void, when )
import qualified Data.Text as T
import Data.Tree ( Tree(..) )
import "gtk3" Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified "gtk3" Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import Text.Read ( readMaybe )

data FieldElem =
  FieldElem
  { feName :: Maybe String
  , feUpstreamField :: DField
  , feStagedField :: DField
  } deriving Show

data ConstructorElem =
  ConstructorElem
  { ceName :: Maybe String
  , ceDName :: String
  , ceCName :: String
  } deriving (Show, Eq)

data SumElem =
  SumElem
  { seName :: Maybe String
  , seDName :: String
  , seUpstreamSum :: DSimpleEnum
  , seStagedSum :: DSimpleEnum
  , seListStore :: Gtk.ListStore String
--  , seSpinAdjustment :: Gtk.Adjustment
  }
instance Show SumElem where
  show se@(SumElem {}) =
    init $
    unlines
    [ "SumElem"
    , "{ seName = " ++ show (seName se)
    , ", seDName = " ++ seDName se
    , ", seUpstreamSum = " ++ show (seUpstreamSum se)
    , ", seStagedSum = " ++ show (seStagedSum se)
    , "}"
    ]

data ListViewElement =
  LveField FieldElem
  | LveConstructor ConstructorElem
  | LveSum SumElem
  deriving Show

ddataToTree :: Maybe String -> Either DField DData -> IO (Tree ListViewElement)
ddataToTree name (Left field) = return $ Node (LveField fe) []
  where
    fe =
      FieldElem
      { feName = name
      , feUpstreamField = field
      , feStagedField = field
      }
ddataToTree name (Right (DData dname (DConstructor cname fields))) = do
  let ce =
        ConstructorElem
        { ceName = name
        , ceDName = dname
        , ceCName = cname
        }
  children <- mapM (uncurry ddataToTree) fields
  return $ Node (LveConstructor ce) children
ddataToTree name (Right (DData dname (DSum s))) = do
  -- Dummy list store. We'll set the options ourselves in the editingStarted signal
  listStore <- Gtk.listStoreNew []
  Gtk.treeModelSetColumn listStore (Gtk.makeColumnIdString 0) id
--  let value = 0
--      lower = 0
--      upper = realToFrac (length options - 1)
--      stepIncrement = 1
--      pageIncrement = 1
--      pageSize = 0
--  adjustment <- Gtk.adjustmentNew value lower upper stepIncrement pageIncrement pageSize
  let se =
        SumElem
        { seName = name
        , seDName = dname
        , seUpstreamSum = s
        , seStagedSum = s
        , seListStore = listStore
--        , seSpinAdjustment = adjustment
        }
  return $ Node (LveSum se) []

treeToStagedDData :: Tree ListViewElement -> Either DField DData
treeToStagedDData (Node (LveField fe) []) = Left (feStagedField fe)
treeToStagedDData (Node (LveField fe) _) =
  error $ "treeToStagedDData: LveField " ++ show fe ++ " has children"
treeToStagedDData (Node (LveConstructor ce) fields) =
  Right (DData dname (DConstructor cname (map f fields)))
  where
    dname = ceDName ce
    cname = ceCName ce

    getName :: Tree ListViewElement -> Maybe String
    getName (Node (LveSum se) _) = seName se
    getName (Node (LveConstructor ce') _) = ceName ce'
    getName (Node (LveField fe) _) = feName fe

    f x = (getName x, treeToStagedDData x)
treeToStagedDData (Node (LveSum se) []) =
  Right (DData dname (DSum s))
  where
    dname = seDName se
    s = seStagedSum se
treeToStagedDData (Node (LveSum _) _) =
  error $ "treeToStagedDData: LveSum has children"


treeToUpstreamDData :: Tree ListViewElement -> Either DField DData
treeToUpstreamDData (Node (LveField fe) []) = Left (feUpstreamField fe)
treeToUpstreamDData (Node (LveField fe) _) =
  error $ "treeToUpstreamDData: LveField " ++ show fe ++ " has children"
treeToUpstreamDData (Node (LveConstructor ce) fields) =
  Right (DData dname (DConstructor cname (map f fields)))
  where
    dname = ceDName ce
    cname = ceCName ce

    getName :: Tree ListViewElement -> Maybe String
    getName (Node (LveSum se) _) = seName se
    getName (Node (LveConstructor ce') _) = ceName ce'
    getName (Node (LveField fe) _) = feName fe

    f x = (getName x, treeToUpstreamDData x)
treeToUpstreamDData (Node (LveSum se) []) =
  Right (DData dname (DSum s))
  where
    dname = seDName se
    s = seUpstreamSum se
treeToUpstreamDData (Node (LveSum _) _) =
  error $ "treeToUpstreamDData: LveSum has children"


newLookupTreeview ::
  (Double -> String)
  -> (Float -> String)
  -> String
  -> DTree
  -> IO Bool
  -> (DTree -> IO ())
  -> IO (Gtk.TreeView, IO DTree, DTree -> IO (), IO (), DTree -> IO (), IO ())
newLookupTreeview showDouble showFloat rootName initialValue getAutocommit commit = do
  treeStore <- Gtk.treeStoreNew [] :: IO (Gtk.TreeStore ListViewElement)
  treeview <- Gtk.treeViewNewWithModel treeStore :: IO Gtk.TreeView

  Gtk.treeViewSetHeadersVisible treeview True
  Gtk.treeViewSetEnableTreeLines treeview True
--  Gtk.treeViewSetGridLines treeview Gtk.TreeViewGridLinesVertical
--  Gtk.treeViewSetGridLines treeview Gtk.TreeViewGridLinesBoth

  -- add some columns
  colName <- Gtk.treeViewColumnNew
  colType <- Gtk.treeViewColumnNew
  colUpstreamValue <- Gtk.treeViewColumnNew
  colStagedValue <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle colName "name"
  Gtk.treeViewColumnSetTitle colType "type"
  Gtk.treeViewColumnSetTitle colUpstreamValue "upstream"
  Gtk.treeViewColumnSetTitle colStagedValue "staged"

  rendererName <- Gtk.cellRendererTextNew
  rendererType <- Gtk.cellRendererTextNew
  rendererStagedValue <- Gtk.cellRendererTextNew
  rendererUpstreamValue <- Gtk.cellRendererTextNew
  rendererCombo <- Gtk.cellRendererComboNew

  Gtk.treeViewColumnPackStart colName rendererName True
  Gtk.treeViewColumnPackStart colType rendererType True
  Gtk.treeViewColumnPackStart colUpstreamValue rendererUpstreamValue True
  Gtk.treeViewColumnPackStart colStagedValue rendererStagedValue True
  Gtk.treeViewColumnPackStart colStagedValue rendererCombo True

  _ <- Gtk.treeViewAppendColumn treeview colName
  _ <- Gtk.treeViewAppendColumn treeview colType
  _ <- Gtk.treeViewAppendColumn treeview colUpstreamValue
  _ <- Gtk.treeViewAppendColumn treeview colStagedValue

  -- add an extra dummy column at the end so that the staged value doesn't
  -- expand to the right
  _ <- Gtk.treeViewColumnNew >>= Gtk.treeViewAppendColumn treeview

  ----------------------------- showing values ------------------------
  -- data name
  let showName :: ListViewElement -> String
      showName (LveSum se) = fromMName $ seName se
      showName (LveField fe) = fromMName $ feName fe
      showName (LveConstructor ce) = fromMName $ ceName ce
      fromMName (Just r) = r
      fromMName Nothing = "()"

  Gtk.cellLayoutSetAttributes colName rendererName treeStore $
    \lve -> [Gtk.cellText := showName lve]

  -- data type
  let showType :: ListViewElement -> String
      showType (LveSum se) = seDName se
      showType (LveConstructor ce) = ceDName ce
      showType (LveField fe) = describeDField (feStagedField fe)

  Gtk.cellLayoutSetAttributes colType rendererType treeStore $
        \lve -> [ Gtk.cellText := showType lve ]

  -- upstream
  let showField :: DField -> String
      showField (DDouble x) = showDouble x
      showField (DFloat x) = showFloat x
      showField (DInt8 x) = show x
      showField (DInt16 x) = show x
      showField (DInt32 x) = show x
      showField (DInt64 x) = show x
      showField (DWord8 x) = show x
      showField (DWord16 x) = show x
      showField (DWord32 x) = show x
      showField (DWord64 x) = show x
      showField (DString x) = x
      showField DSorry = ""
      showField DUnit = "()"

      showSum :: DSimpleEnum -> String
      showSum denum = case denumToString denum of
        Left msg -> msg
        Right r -> r

  -- upstream
  Gtk.cellLayoutSetAttributes colUpstreamValue rendererUpstreamValue treeStore $
    \lve -> case lve of
      LveField fe -> [ Gtk.cellText := showField (feUpstreamField fe)
                     , Gtk.cellTextEditable := False
                     ]
      LveSum se -> [ Gtk.cellText := showSum (seUpstreamSum se)
                   , Gtk.cellTextEditable := False
                   ]
      LveConstructor _ -> [ Gtk.cellText := ""
                          , Gtk.cellTextEditable := False
                          ]

  -- staged
  Gtk.cellLayoutSetAttributes colStagedValue rendererStagedValue treeStore $
    \lve -> case lve of
      LveField fe ->
        [ Gtk.cellVisible := True
        , Gtk.cellText := showField (feStagedField fe)
        , Gtk.cellTextEditable := True
        ]
      LveSum _ ->
        [ Gtk.cellVisible := False
        ]
      LveConstructor _ ->
        [ Gtk.cellVisible := False
        , Gtk.cellTextEditable := False
        ]

  -- combo box
  Gtk.cellLayoutSetAttributes colStagedValue rendererCombo treeStore $ \lve ->
    case lve of
      LveSum (SumElem {seStagedSum = denum, seListStore = listStore}) ->
        [ Gtk.cellVisible := True
        , Gtk.cellComboHasEntry := False
        , Gtk.cellTextEditable := True
        , Gtk.cellComboTextModel := (listStore, Gtk.makeColumnIdString 0 :: Gtk.ColumnId String String)
        , Gtk.cellText := denumToStringOrMsg denum
        ]
      _ -> [ Gtk.cellMode := Gtk.CellRendererModeInert
           , Gtk.cellVisible := False
           ]
  -- workaround to make sure combo box is displaying correctly, see
  -- http://stackoverflow.com/questions/35147473/cant-get-gtk2-cellrenderercombo-to-display-anything
  _ <- on rendererCombo Gtk.editingStarted $ \widget treePath -> do
    lve <- Gtk.treeStoreGetValue treeStore treePath
    case lve of
      LveField _ -> error "Combo renderer is Field"
      LveConstructor _ -> error "Combo renderer is Constructor"
      LveSum se -> do
        let comboBox = Gtk.castToComboBox widget
        comboListStore <- Gtk.comboBoxSetModelText comboBox
        let DSimpleEnum constructors active = seStagedSum se
        mapM_ (Gtk.listStoreAppend comboListStore . T.pack) constructors
        Gtk.comboBoxSetActive comboBox active


  --------------------------- getting/setting the whole tree --------------------------
  let mergeTrees :: Gtk.TreePath -> Tree ListViewElement -> IO ()
      mergeTrees treePath newTree = do
        moldTree <- Gtk.treeStoreLookup treeStore treePath
        oldTree <- case moldTree of
          Nothing -> error "failed looking up treestore"
          Just r -> return r
          :: IO (Tree ListViewElement)

        let assertCompatible :: Show a => Maybe String -> a -> a -> Bool -> IO ()
            assertCompatible _ _ _ True = return ()
            assertCompatible mdesc oldNode newNode False =
              error $
              unlines $
              [ "the \"impossible\" happened: trees aren't compatible"
              ,"old tree:"
              , show oldTree
              , ""
              , "new tree:"
              , show newTree
              , ""
              , "old node:"
              , show oldNode
              , ""
              , "new node:"
              , show newNode
              ] ++ case mdesc of
                Nothing -> []
                Just desc -> ["", desc]

        case (oldTree, newTree) of
          -- sums
          (Node oldNode@(LveSum oldSum) [], Node newNode@(LveSum newSum) []) -> do
            let (compatible, mergedSum) = mergeSums oldSum newSum
            assertCompatible Nothing oldNode newNode compatible
            changed <- Gtk.treeStoreChange treeStore treePath (const (LveSum mergedSum))
            case changed of
              False -> error $ "merged sums didn't change"
              True -> return ()
          (oldNode, newNode@(Node (LveSum _) [])) ->
            assertCompatible (Just "got a mixed type") oldNode newNode False
          (_, Node (LveSum _) _) -> error "mergeTrees: new LveSum has children"

          -- fields
          (Node oldNode@(LveField oldField) [], Node newNode@(LveField newField) []) -> do
            let (compatible, mergedField) = mergeFields oldField newField
            assertCompatible Nothing oldNode newNode compatible
            changed <- Gtk.treeStoreChange treeStore treePath (const (LveField mergedField))
            case changed of
              False -> error $ "merged fields didn't change"
              True -> return ()
          (oldNode, newNode@(Node (LveField _) [])) ->
            assertCompatible (Just "got a mixed type") oldNode newNode False
          (_, Node (LveField _) _) -> error "mergeTrees: new LveField has children"

          -- constructors
          (Node oldNode@(LveConstructor oldCons) oldChildren, Node newNode@(LveConstructor newCons) newChildren)
            | oldCons /= newCons -> assertCompatible Nothing oldNode newNode False
            | length oldChildren /= length newChildren ->
                assertCompatible (Just "constructor length mismatch") oldNode newNode False
            | otherwise -> do
                mtreeIter <- Gtk.treeModelGetIter treeStore treePath
                treeIter <- case mtreeIter of
                  Nothing -> error "error looking up tree iter"
                  Just r -> return r
                nchildren <- Gtk.treeModelIterNChildren treeStore (Just treeIter)
                void $ when (nchildren /= length oldChildren) $
                  error "error $ treeModelIterNChildren /= length oldChildren"
                let mergeNthChild _ [] = return ()
                    mergeNthChild k (newChild:others) = do
                      mtreeIter' <- Gtk.treeModelGetIter treeStore treePath
                      treeIter' <- case mtreeIter' of
                        Nothing -> error "error looking up tree iter"
                        Just r -> return r
                      mchildIter <- Gtk.treeModelIterNthChild treeStore (Just treeIter') k
                      childIter <- case mchildIter of
                        Nothing -> error "treeModelIterNthChild failed"
                        Just r -> return r
                      mchildPath <- Gtk.treeModelGetPath treeStore childIter
                      childPath <- case mchildPath of
                        [] -> error "child TreePath is invalid"
                        r -> return r
                      mergeTrees childPath newChild
                      mergeNthChild (k + 1) others
                mergeNthChild 0 newChildren
          (oldNode, newNode@(Node (LveConstructor _) _)) ->
            assertCompatible (Just "got a mixed type") oldNode newNode False

      receiveNewUpstream :: DTree -> IO ()
      receiveNewUpstream newMsg = do
        moldTree <- Gtk.treeStoreLookup treeStore [0]
        oldTree <- case moldTree of
          Nothing -> error "failed looking up old treestore"
          Just r -> return r

        newTree <- ddataToTree (Just rootName) newMsg :: IO (Tree ListViewElement)
        let (compatible, mergedTree) = compatibleTrees oldTree newTree
        if compatible
          then mergeTrees [0] newTree -- merge in place so that the expando doesn't collapse
          else do
            putStrLn "settings app rebuilding tree..."
            Gtk.treeStoreClear treeStore
            Gtk.treeStoreInsertTree treeStore [] 0 mergedTree

      getLatestStaged = do
        mtree <- Gtk.treeStoreLookup treeStore [0]
        case mtree of
          Nothing -> error "failed looking up treestore"
          Just r -> return (treeToStagedDData r)

      getLatestUpstream = do
        mtree <- Gtk.treeStoreLookup treeStore [0]
        return $ case mtree of
         Nothing -> Nothing
         Just r -> Just (treeToUpstreamDData r)

      loadDTree :: DTree -> IO ()
      loadDTree dtree = do
        eupstream <- getLatestUpstream
        Gtk.treeStoreClear treeStore
        tree <- ddataToTree (Just rootName) dtree
        Gtk.treeStoreInsertTree treeStore [] 0 tree
        case eupstream of
          Nothing -> return ()
          Just upstream -> receiveNewUpstream upstream

      takeLatestUpstream = do
        let takeUpstreamElem :: ListViewElement -> ListViewElement
            takeUpstreamElem (LveField fe) = LveField (fe {feStagedField = feUpstreamField fe})
            takeUpstreamElem c@(LveConstructor _) = c
            takeUpstreamElem (LveSum se) = LveSum (se {seStagedSum = seUpstreamSum se})

            takeUpstream :: Gtk.TreeIter -> IO ()
            takeUpstream iter = do
              -- change the value at the current iter
              path <- Gtk.treeModelGetPath treeStore iter
              _ <- Gtk.treeStoreChange treeStore path takeUpstreamElem

              -- if there are children, change their values
              mchildren <- Gtk.treeModelIterChildren treeStore iter
              case mchildren of
                Just childIter -> takeUpstream childIter
                Nothing -> return ()

              -- if there are siblings, change their values
              msiblings <- Gtk.treeModelIterNext treeStore iter
              case msiblings of
                Just siblingIter -> takeUpstream siblingIter
                Nothing -> return ()

        miter <- Gtk.treeModelGetIterFirst treeStore
        case miter of
          Nothing -> return ()
          Just root -> takeUpstream root


  -------------------------- editing values ----------------------
  let sendDataIfAutocommitIsOn = do
        autoCommit <- getAutocommit
        when autoCommit (getLatestStaged >>= commit)

      modifyField :: DField -> String -> DField
      modifyField f0@(DDouble _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DDouble x
      modifyField f0@(DFloat _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DFloat x
      modifyField f0@(DInt8 _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DInt8 x
      modifyField f0@(DInt16 _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DInt16 x
      modifyField f0@(DInt32 _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DInt32 x
      modifyField f0@(DInt64 _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DInt64 x
      modifyField f0@(DWord8 _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DWord8 x
      modifyField f0@(DWord16 _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DWord16 x
      modifyField f0@(DWord32 _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DWord32 x
      modifyField f0@(DWord64 _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DWord64 x
      modifyField (DString _) txt = DString txt
      modifyField DSorry _ = DSorry
      modifyField DUnit _ = DUnit

      modifySum :: DSimpleEnum -> String -> DSimpleEnum
      modifySum denum txt = case denumSetString denum txt of
        Left _ -> denum
        Right r -> r

  _ <- on rendererStagedValue Gtk.edited $ \treePath txt -> do
    lve0 <- Gtk.treeStoreGetValue treeStore treePath
    let lve = case lve0 of
          LveField fe -> LveField (fe {feStagedField = modifyField (feStagedField fe) txt})
          LveSum se -> LveSum (se {seStagedSum = modifySum (seStagedSum se) txt})
          ce@(LveConstructor _) -> ce -- not editible anyway
    Gtk.treeStoreSetValue treeStore treePath lve
    sendDataIfAutocommitIsOn


  _ <- on rendererCombo Gtk.edited $ \treePath newVal -> do
    lve0 <- Gtk.treeStoreGetValue treeStore treePath
    let newLve = case lve0 of
          LveSum se -> LveSum (se {seStagedSum = case denumSetString (seStagedSum se) newVal of
                                    Left msg -> error $ "error updating sum elem: " ++ msg
                                    Right r -> r
                                  })
          LveField _ -> error "cell renderer edited on Field"
          LveConstructor _ -> error "cell renderer edited on Constructor"
    Gtk.treeStoreSetValue treeStore treePath newLve
    sendDataIfAutocommitIsOn


  Gtk.treeStoreClear treeStore
  tree <- ddataToTree (Just rootName) initialValue
  Gtk.treeStoreInsertTree treeStore [] 0 tree

  let printDiff = do
        mupstream <- getLatestUpstream :: IO (Maybe (Either DField DData))
        staged <- getLatestStaged :: IO (Either DField DData)
        case mupstream of
          Just upstream -> case diffDTrees rootName upstream staged of
            [] -> putStrLn "======== all fields match! ======="
            msgs -> putStrLn "====== some fields don't match! =====" >> mapM_ putStrLn msgs
          Nothing -> putStrLn "===== no staged data ===="

  return (treeview, getLatestStaged, receiveNewUpstream, takeLatestUpstream, loadDTree, printDiff)

mergeSums :: SumElem -> SumElem -> (Bool, SumElem)
mergeSums oldSum newSum
  | seName oldSum /= seName newSum = (False, newSum)
  | seDName oldSum /= seDName newSum = (False, newSum)
  | oldOptions /= newOptions = (False, newSum)
  | otherwise = (True, newSum {seStagedSum = seStagedSum oldSum})
  where
    DSimpleEnum oldOptions _ = seStagedSum oldSum
    DSimpleEnum newOptions _ = seStagedSum newSum

mergeFields :: FieldElem -> FieldElem -> (Bool, FieldElem)
mergeFields oldElem newElem
  | feName oldElem /= feName newElem = (False, newElem)
  | not (sameDFieldType oldField newField) = (False, newElem)
  | otherwise = (True, newElem {feStagedField = oldField})
  where
    oldField = feStagedField oldElem
    newField = feStagedField newElem

-- return the merged trees and a flag saying if the trees have the same structure
compatibleTrees :: Tree ListViewElement -> Tree ListViewElement -> (Bool, Tree ListViewElement)
-- sums
compatibleTrees (Node (LveSum oldSum) []) (Node (LveSum newSum) []) =
  (compatible, Node (LveSum merged) [])
  where
    (compatible, merged) = mergeSums oldSum newSum
compatibleTrees _ newNode@(Node (LveSum _) []) = (False, newNode)
compatibleTrees _ (Node (LveSum _) _) = error "compatibleTrees: new LveSum has children"
-- fields
compatibleTrees (Node (LveField oldField) []) (Node (LveField newField) []) =
  (compatible, Node (LveField merged) [])
  where
    (compatible, merged) = mergeFields oldField newField
compatibleTrees _ newNode@(Node (LveField _) []) = (False, newNode)
compatibleTrees _ (Node (LveField _) _) = error "compatibleTrees: new LveField has children"
-- constructors
compatibleTrees (Node (LveConstructor oldCons) _) newNode@(Node (LveConstructor newCons) _)
  | oldCons /= newCons = (False, newNode)
compatibleTrees (Node (LveConstructor _) oldChildren) (Node (LveConstructor newCons) newChildren) =
  (childrenCompatible, Node (LveConstructor newCons) mergedChildren)
  where
    (childrenCompatible, mergedChildren) = mergeChildren newChildren

    mergeChild :: Tree ListViewElement -> (Bool, Tree ListViewElement)
    mergeChild newChild = tryOldChildren oldChildren
      where
        tryOldChildren [] = (False, newChild)
        tryOldChildren (oldChild:others)
          | compatible = (True, mergedChild)
          | otherwise = tryOldChildren others
          where
            (compatible, mergedChild) = compatibleTrees oldChild newChild

    mergeChildren :: [Tree ListViewElement] -> (Bool, [Tree ListViewElement])
    mergeChildren (newChild:others) = (childCompatible && othersCompatible, mergedChild:mergedOthers)
      where
        (childCompatible, mergedChild) = mergeChild newChild
        (othersCompatible, mergedOthers) = mergeChildren others
    mergeChildren [] = (True, [])
compatibleTrees _ newNode@(Node (LveConstructor _) _) = (False, newNode)
