{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SetHo.LookupTree
       ( ListViewElement(..)
       , newLookupTreeview
       ) where

import Accessors.Dynamic
       ( DTree, DData(..), DConstructor(..), DSimpleEnum(..), DField(..)
       , describeDField, sameDFieldType
       )
import Control.Monad ( void, when )
import Data.Tree ( Tree(..) )
import Graphics.UI.Gtk ( AttrOp( (:=) ) )
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Signals ( on )
import Text.Read ( readMaybe )
import Text.Printf ( printf )

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
  } deriving Show

data ListViewElement =
  LveField FieldElem
  | LveConstructor ConstructorElem
  | LveSum SumElem
  deriving Show

ddataToTree :: Maybe String -> Either DField DData -> Tree ListViewElement
ddataToTree name (Left field) = Node (LveField fe) []
  where
    fe =
      FieldElem
      { feName = name
      , feUpstreamField = field
      , feStagedField = field
      }
ddataToTree name (Right (DData dname (DConstructor cname fields))) =
  Node (LveConstructor ce) (map (uncurry ddataToTree) fields)
  where
    ce =
      ConstructorElem
      { ceName = name
      , ceDName = dname
      , ceCName = cname
      }
ddataToTree name (Right (DData dname (DSum s))) =
  Node (LveSum se) []
  where
    se =
      SumElem
      { seName = name
      , seDName = dname
      , seUpstreamSum = s
      , seStagedSum = s
      }

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
treeToStagedDData (Node (LveSum se) _) =
  error $ "treeToStagedDData: LveSum " ++ show se ++ " has children"

newLookupTreeview ::
  String
  -> DTree
  -> IO (Gtk.ScrolledWindow, IO DTree, DTree -> IO ())
newLookupTreeview rootName initialValue = do
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
  colCombo <- Gtk.treeViewColumnNew

  Gtk.treeViewColumnSetTitle colName "name"
  Gtk.treeViewColumnSetTitle colType "type"
  Gtk.treeViewColumnSetTitle colUpstreamValue "upstream"
  Gtk.treeViewColumnSetTitle colStagedValue "staged"
  Gtk.treeViewColumnSetTitle colCombo "combo"

  rendererName <- Gtk.cellRendererTextNew
  rendererType <- Gtk.cellRendererTextNew
  rendererStagedValue <- Gtk.cellRendererTextNew
  rendererUpstreamValue <- Gtk.cellRendererTextNew
  rendererCombo <- Gtk.cellRendererComboNew

  Gtk.cellLayoutPackStart colName    rendererName True
  Gtk.cellLayoutPackStart colType    rendererType True
  Gtk.cellLayoutPackStart colUpstreamValue rendererUpstreamValue True
  Gtk.cellLayoutPackStart colStagedValue   rendererStagedValue True
  Gtk.cellLayoutPackStart colCombo    rendererCombo True

  _ <- Gtk.treeViewAppendColumn treeview colName
  _ <- Gtk.treeViewAppendColumn treeview colType
  _ <- Gtk.treeViewAppendColumn treeview colUpstreamValue
  _ <- Gtk.treeViewAppendColumn treeview colStagedValue
  _ <- Gtk.treeViewAppendColumn treeview colCombo

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

  let showField :: DField -> String
      showField (DDouble x) = printf "%.2g" x
      showField (DFloat x) = printf "%.2g" x
      showField (DInt x) = show x
      showField (DString x) = x
      showField DSorry = ""

      showSum :: DSimpleEnum -> String
      showSum (DSimpleEnum names k) = case safeIndex names k of
        Just n -> n
        Nothing -> "<out of bounds>"
        where
          safeIndex (x:_) 0 = Just x
          safeIndex (_:xs) j = safeIndex xs (j-1)
          safeIndex [] _ = Nothing


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

  Gtk.cellLayoutSetAttributes colStagedValue rendererStagedValue treeStore $
    \lve -> case lve of
      LveField fe ->
        [ Gtk.cellText := showField (feStagedField fe)
        , Gtk.cellTextEditable := True
        ]
      LveSum se ->
        [ Gtk.cellText := showSum (seStagedSum se)
        , Gtk.cellTextEditable := True
        ]
      LveConstructor _ ->
        [ Gtk.cellText := ""
        , Gtk.cellTextEditable := False
        ]

  let modifyField :: DField -> String -> DField
      modifyField f0@(DDouble _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DDouble x
      modifyField f0@(DFloat _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DFloat x
      modifyField f0@(DInt _) txt = case readMaybe txt of
        Nothing -> f0
        Just x -> DInt x
      modifyField (DString _) txt = DString txt
      modifyField DSorry _ = DSorry

      modifySum :: DSimpleEnum -> String -> DSimpleEnum
      modifySum (DSimpleEnum options k0) txt = DSimpleEnum options k1
        where
          k1 = case safeLookup options 0 of
            Nothing -> k0
            Just r -> r
          safeLookup (opt:opts) j
            | opt == txt = Just j
            | otherwise = safeLookup opts (j+1)
          safeLookup [] _ = Nothing

  _ <- on rendererStagedValue Gtk.edited $ \treePath txt -> do
    let _ = txt :: String
    lve0 <- Gtk.treeStoreGetValue treeStore treePath
    let lve = case lve0 of
          LveField fe -> LveField (fe {feStagedField = modifyField (feStagedField fe) txt})
          LveSum se -> LveSum (se {seStagedSum = modifySum (seStagedSum se) txt})
          ce@(LveConstructor _) -> ce -- not editible anyway
    Gtk.treeStoreSetValue treeStore treePath lve

--  -- bool
--  let toShownBool marked (Just (FieldBool _)) =
--         [ Gtk.cellToggleInconsistent := False
--         , Gtk.cellToggleActive := marked
--         , Gtk.cellToggleActivatable := True
--         , Gtk.cellToggleRadio := True
--         , Gtk.cellToggleIndicatorSize := 12
--         ]
--      toShownBool _ _ =
--         [ Gtk.cellToggleInconsistent := True
--         , Gtk.cellToggleActive := False
--         , Gtk.cellToggleActivatable := False
--         , Gtk.cellToggleRadio := True
--         , Gtk.cellToggleIndicatorSize := 0
--         ]
--
--  Gtk.cellLayoutSetAttributes colBool rendererBool treeStore $
--        \lve -> toShownBool (lveMarked lve) (lveField lve)

-------------   _ <- on rendererBool Gtk.cellToggled $ \pathStr -> do
-------------     let treePath = Gtk.stringToTreePath pathStr
-------------     lve0 <- Gtk.treeStoreGetValue treeStore treePath
-------------     let newMarked :: Bool
-------------         newMarked = not (lveMarked lve0)
-------------         newMutator :: a -> a
-------------         newMutator = case lveField lve0 of
------------- --          Just (FieldBool f) -> f .~ newMarked
-------------           Just f -> error $ "the new mutator must be a bool mutator, got "
-------------                     ++ describeDField f
-------------           Nothing -> error "the new mutator must be not Nothing"
-------------     Gtk.treeStoreSetValue treeStore treePath
-------------       (lve0 {lveMarked = newMarked, lveStagedValue = newMutator})
--    return ()

  -- combo box
  let toCombo _lve = []
  Gtk.cellLayoutSetAttributes colCombo rendererCombo treeStore toCombo

  Gtk.treeStoreClear treeStore
  Gtk.treeStoreInsertTree treeStore [] 0 (ddataToTree (Just rootName) initialValue)

--  let forEach :: (ListViewElement -> IO ListViewElement) -> IO ()
--      forEach f = Gtk.treeModelForeach treeStore $ \treeIter -> do
--         treePath <- Gtk.treeModelGetPath treeStore treeIter
--         lve0 <- Gtk.treeStoreGetValue treeStore treePath
--         lve1 <- f lve0
--         Gtk.treeStoreSetValue treeStore treePath lve1
--         return False

--  let gotNewValue val = do
--        moldTree <- Gtk.treeStoreLookup treeStore [0]
--        oldTree <- case moldTree of
--          Nothing -> error "failed looking up treestore"
--          Just r -> return r
--
--        -- forEach (\lve -> return (lve {lveUpstreamValue = val}))
--        return ()

--  -- on insert or change, rebuild the signal tree
--  _ <- on treeStore Gtk.rowChanged $ \_ changedPath -> do
--    newMsg <- Gtk.listStoreGetValue msgStore (Gtk.listStoreIterToIndex changedPath)
--    gotNewValue newMsg
--
--  _ <- on treeStore Gtk.rowInserted $ \_ changedPath -> do
--    newMsg <- Gtk.listStoreGetValue msgStore (Gtk.listStoreIterToIndex changedPath)
--    gotNewValue newMsg

  scroll <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.containerAdd scroll treeview
  Gtk.set scroll [ Gtk.scrolledWindowHscrollbarPolicy := Gtk.PolicyNever
                 , Gtk.scrolledWindowVscrollbarPolicy := Gtk.PolicyAutomatic
                 ]

  let mergeTrees :: Gtk.TreePath -> Tree ListViewElement -> IO ()
      mergeTrees treePath newTree = do
        moldTree <- Gtk.treeStoreLookup treeStore treePath
        oldTree <- case moldTree of
          Nothing -> error "failed looking up treestore"
          Just r -> return r
          :: IO (Tree ListViewElement)

        let assertCompatible :: Bool -> IO ()
            assertCompatible False = error "the \"impossible\" happened: trees aren't compatible"
            assertCompatible True = return ()

        case (oldTree, newTree) of
          -- sums
          (Node (LveSum oldSum) [], Node (LveSum newSum) []) -> do
            let (compatible, mergedSum) = mergeSums oldSum newSum
            assertCompatible compatible
            changed <- Gtk.treeStoreChange treeStore treePath (const (LveSum mergedSum))
            case changed of
              False -> error $ "merged sums didn't change"
              True -> return ()
          (_, Node (LveSum _) []) -> assertCompatible False
          (_, Node (LveSum _) _) -> error "mergeTrees: new LveSum has children"

          -- fields
          (Node (LveField oldField) [], Node (LveField newField) []) -> do
            let (compatible, mergedField) = mergeFields oldField newField
            assertCompatible compatible
            changed <- Gtk.treeStoreChange treeStore treePath (const (LveField mergedField))
            case changed of
              False -> error $ "merged fields didn't change"
              True -> return ()
          (_, Node (LveField _) []) -> assertCompatible False
          (_, Node (LveField _) _) -> error "mergeTrees: new LveField has children"

          -- constructors
          (Node (LveConstructor oldCons) oldChildren, Node (LveConstructor newCons) newChildren)
            | oldCons /= newCons -> assertCompatible False
            | length oldChildren /= length newChildren -> assertCompatible False
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
          (_, Node (LveConstructor _) _) -> assertCompatible False

      receiveNewValue :: DTree -> IO ()
      receiveNewValue newMsg = do
        moldTree <- Gtk.treeStoreLookup treeStore [0]
        oldTree <- case moldTree of
          Nothing -> error "failed looking up old treestore"
          Just r -> return r

        let newTree :: Tree ListViewElement
            newTree = ddataToTree (Just rootName) newMsg
            (compatible, mergedTree) = compatibleTrees oldTree newTree
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

  return (scroll, getLatestStaged, receiveNewValue)

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
