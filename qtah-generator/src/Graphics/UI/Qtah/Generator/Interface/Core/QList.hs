-- This file is part of Qtah.
--
-- Copyright 2015-2018 The Qtah Authors.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}

-- | Bindings for @QList@.
module Graphics.UI.Qtah.Generator.Interface.Core.QList (
  -- * Template
  Options (..),
  defaultOptions,
  Contents (..),
  inheritHasContents,
  instantiate,
  -- * Instantiations
  allModules,
  c_QListInt,
  c_QListQAbstractButton,
  c_QListQByteArray,
  c_QListQItemSelectionRange,
  c_QListQModelIndex,
  c_QListQObject,
  c_QListQSize,
  c_QListQString,
  c_QListQVariant,
  c_QListQWidget,
  ) where

import Control.Monad (forM_, when)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat, mempty)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  Generator,
  HsTypeSide (HsHsSide),
  addImports,
  cppTypeToHsTypeAndUse,
  indent,
  ln,
  prettyPrint,
  sayLn,
  saysLn,
  toHsCastMethodName,
  toHsClassEntityName',
  toHsDataTypeName,
  )
import Foreign.Hoppy.Generator.Spec (
  Class,
  ClassHaskellConversion (
    ClassHaskellConversion,
    classHaskellConversionFromCppFn,
    classHaskellConversionToCppFn,
    classHaskellConversionType
    ),
  Constness (Const, Nonconst),
  Export (ExportClass),
  Operator (OpAdd, OpArray),
  Reqs,
  Type,
  addReqs,
  addAddendumHaskell,
  classSetEntityPrefix,
  classSetHaskellConversion,
  classSetMonomorphicSuperclass,
  hsImport1,
  identT,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  reqInclude,
  toExtName,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, constT, intT, objT, ptrT, refT, toGcT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionRange (c_QItemSelectionRange)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Imports
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (
  c_QAbstractButton,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), QtModule, makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsQName (Special),
  HsSpecialCon (HsListCon),
  HsType (HsTyApp, HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | Options for instantiating the list classes.
newtype Options = Options
  { optListClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @QList@ class.  Lists are always
    -- 'Assignable' and 'Copyable', but you may want to add 'Equatable' if your
    -- value type supports it.
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options []

-- | A set of instantiated classes.
newtype Contents = Contents
  { c_QList :: Class  -- ^ @QList\<T>@
  }

-- | @instantiate className t tReqs@ creates a set of bindings for an
-- instantiation of @QList@ and associated types (e.g. iterators).  In the
-- result, the 'c_QList' class has an external name of @className@.
instantiate :: String -> Type -> Reqs -> Contents
instantiate listName t tReqs = instantiate' listName t tReqs defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Reqs -> Options -> Contents
instantiate' listName t tReqs opts =
  let reqs = mconcat [ tReqs
                     , reqInclude $ includeStd "QList"
                     ]
      features = Assignable : Copyable : optListClassFeatures opts
      hasReserve = qtVersion >= [4, 7]

      list =
        addReqs reqs $
        addAddendumHaskell addendum $
        classSetHaskellConversion conversion $
        classAddFeatures features $
        classSetMonomorphicSuperclass $
        classSetEntityPrefix "" $
        makeClass (identT "QList" [t]) (Just $ toExtName listName) [] $ collect
        [ just $ mkCtor "new" []
        , just $ mkMethod' "append" "append" [t] voidT
        , test (qtVersion >= [4, 5]) $ mkMethod' "append" "appendList" [objT list] voidT
        , just $ mkMethod' OpArray "at" [intT] $ refT t
        , just $ mkConstMethod' "at" "atConst" [intT] $ refT $ constT t
          -- OMIT back
          -- OMIT begin
        , just $ mkMethod "clear" [] voidT
        , just $ mkConstMethod "contains" [t] boolT
          -- OMIT count()
        , just $ mkConstMethod "count" [t] intT
        , test (qtVersion >= [4, 5]) $ mkConstMethod "endsWith" [t] boolT
          -- OMIT erase
        , just $ mkMethod' "first" "first" [] $ refT t
        , just $ mkConstMethod' "first" "firstConst" [] $ refT $ constT t
        , just $ mkConstMethod' OpArray "get" [intT] t
        , just $ mkConstMethod' "indexOf" "indexOf" [t] intT
        , just $ mkConstMethod' "indexOf" "indexOfFrom" [t, intT] intT
        , just $ mkMethod "insert" [intT, t] voidT
        , just $ mkConstMethod "isEmpty" [] boolT
        , just $ mkMethod' "last" "last" [] $ refT t
        , just $ mkConstMethod' "last" "lastConst" [] $ refT $ constT t
        , just $ mkConstMethod' "lastIndexOf" "lastIndexOf" [t] intT
        , just $ mkConstMethod' "lastIndexOf" "lastIndexOfFrom" [t, intT] intT
          -- OMIT length
        , just $ mkConstMethod' "mid" "mid" [intT] $ toGcT $ objT list
        , just $ mkConstMethod' "mid" "midLength" [intT, intT] $ toGcT $ objT list
        , just $ mkMethod "move" [intT, intT] voidT
        , just $ mkMethod "prepend" [t] voidT
        , just $ mkMethod "removeAll" [t] intT
        , just $ mkMethod "removeAt" [intT] voidT
        , just $ mkMethod "removeFirst" [] voidT
        , just $ mkMethod "removeLast" [] voidT
        , test (qtVersion >= [4, 4]) $ mkMethod "removeOne" [t] boolT
        , just $ mkMethod "replace" [intT, t] voidT
        , test hasReserve $ mkMethod "reserve" [intT] voidT
        , just $ mkConstMethod "size" [] intT
        , test (qtVersion >= [4, 5]) $ mkConstMethod "startsWith" [t] boolT
        , just $ mkMethod "swap" [intT, intT] voidT
          -- OMIT swap(QList<T>&)
        , just $ mkMethod "takeAt" [intT] t
        , just $ mkMethod "takeFirst" [] t
        , just $ mkMethod "takeLast" [] t
          -- TODO toSet
          -- TODO toStdList
          -- TODO toVector
        , just $ mkConstMethod' "value" "value" [intT] t
        , just $ mkConstMethod' "value" "valueOr" [intT, t] t
        , just $ mkConstMethod OpAdd [objT list] $ toGcT $ objT list
        ]

      -- The addendum for the list class contains HasContents and FromContents
      -- instances.
      addendum = do
        addImports $ mconcat [hsImport1 "Prelude" "(-)",
                              hsImport1 "Control.Monad" "(<=<)",
                              importForPrelude,
                              importForRuntime]
        when hasReserve $
          addImports $ hsImport1 "Prelude" "($)"

        forM_ [Const, Nonconst] $ \cst -> do
          hsDataTypeName <- toHsDataTypeName cst list
          hsValueType <- cppTypeToHsTypeAndUse HsHsSide $ case cst of
            Const -> constT t
            Nonconst -> t

          -- Generate const and nonconst HasContents instances.
          ln
          saysLn ["instance QtahFHR.HasContents ", hsDataTypeName,
                  " (", prettyPrint hsValueType, ") where"]
          indent $ do
            sayLn "toContents this' = do"
            indent $ do
              let listAt = case cst of
                    Const -> "atConst"
                    Nonconst -> "at"
              saysLn ["size' <- ", toHsClassEntityName' list "size", " this'"]
              saysLn ["QtahP.mapM (QtahFHR.decode <=< ",
                      toHsClassEntityName' list listAt, " this') [0..size'-1]"]

          -- Only generate a nonconst FromContents instance.
          when (cst == Nonconst) $ do
            ln
            saysLn ["instance QtahFHR.FromContents ", hsDataTypeName,
                    " (", prettyPrint hsValueType, ") where"]
            indent $ do
              sayLn "fromContents values' = do"
              indent $ do
                saysLn ["list' <- ", toHsClassEntityName' list "new"]
                when hasReserve $
                  saysLn [toHsClassEntityName' list "reserve",
                          " list' $ QtahFHR.coerceIntegral $ QtahP.length values'"]
                saysLn ["QtahP.mapM_ (", toHsClassEntityName' list "append", " list') values'"]
                sayLn "QtahP.return list'"

      -- A QList of some type converts into a Haskell list of the corresponding
      -- type using HasContents/FromContents.
      conversion =
        ClassHaskellConversion
        { classHaskellConversionType = Just $ do
          hsType <- cppTypeToHsTypeAndUse HsHsSide t
          return $ HsTyApp (HsTyCon $ Special HsListCon) hsType
        , classHaskellConversionToCppFn = Just $ do
          addImports importForRuntime
          sayLn "QtahFHR.fromContents"
        , classHaskellConversionFromCppFn = Just $ do
          addImports importForRuntime
          sayLn "QtahFHR.toContents"
        }

  in Contents
     { c_QList = list
     }

-- | Used in an addendum for a class that inherits from QList, this generates a
-- HasContents instance that uses the HasContents instance on the parent list
-- class.
inheritHasContents :: Class -> Class -> Type -> Generator ()
inheritHasContents cls listClass t =
  forM_ [Const, Nonconst] $ \cst -> do
    hsDataTypeName <- toHsDataTypeName cst cls
    hsValueType <- cppTypeToHsTypeAndUse HsHsSide $ case cst of
      Const -> constT t
      Nonconst -> t
    castToListFn <- toHsCastMethodName cst listClass
    addImports importForRuntime
    saysLn ["instance QtahFHR.HasContents ", hsDataTypeName,
            " (", prettyPrint hsValueType, ") where"]
    indent $ saysLn ["toContents = QtahFHR.toContents . ", castToListFn]

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [QtExport]
toExports m = [QtExport $ ExportClass $ c_QList m]

createModule :: String -> Contents -> QtModule
createModule name contents = makeQtModule ["Core", "QList", name] $ toExports contents

allModules :: [AModule]
allModules =
  map AQtModule
  [ qmod_Int
  , qmod_QAbstractButton
  , qmod_QByteArray
  , qmod_QItemSelectionRange
  , qmod_QModelIndex
  , qmod_QObject
  , qmod_QSize
  , qmod_QString
  , qmod_QVariant
  , qmod_QWidget
  ]

qmod_Int :: QtModule
qmod_Int = createModule "Int" contents_Int

contents_Int :: Contents
contents_Int =
  instantiate "QListInt" intT mempty

c_QListInt :: Class
c_QListInt = c_QList contents_Int

qmod_QAbstractButton :: QtModule
qmod_QAbstractButton = createModule "QAbstractButton" contents_QAbstractButton

contents_QAbstractButton :: Contents
contents_QAbstractButton = instantiate "QListQAbstractButton" (ptrT $ objT c_QAbstractButton) mempty

c_QListQAbstractButton :: Class
c_QListQAbstractButton = c_QList contents_QAbstractButton

qmod_QByteArray :: QtModule
qmod_QByteArray = createModule "QByteArray" contents_QByteArray

contents_QByteArray :: Contents
contents_QByteArray = instantiate "QListQByteArray" (objT c_QByteArray) mempty

c_QListQByteArray :: Class
c_QListQByteArray = c_QList contents_QByteArray

qmod_QItemSelectionRange :: QtModule
qmod_QItemSelectionRange = createModule "QItemSelectionRange" contents_QItemSelectionRange

contents_QItemSelectionRange :: Contents
contents_QItemSelectionRange =
  instantiate "QListQItemSelectionRange" (objT c_QItemSelectionRange) mempty

c_QListQItemSelectionRange :: Class
c_QListQItemSelectionRange = c_QList contents_QItemSelectionRange

qmod_QModelIndex :: QtModule
qmod_QModelIndex = createModule "QModelIndex" contents_QModelIndex

contents_QModelIndex :: Contents
contents_QModelIndex = instantiate "QListQModelIndex" (objT c_QModelIndex) mempty

c_QListQModelIndex :: Class
c_QListQModelIndex = c_QList contents_QModelIndex

qmod_QObject :: QtModule
qmod_QObject = createModule "QObject" contents_QObject

contents_QObject :: Contents
contents_QObject = instantiate "QListQObject" (ptrT $ objT c_QObject) mempty

c_QListQObject :: Class
c_QListQObject = c_QList contents_QObject

qmod_QSize :: QtModule
qmod_QSize = createModule "QSize" contents_QSize

contents_QSize :: Contents
contents_QSize = instantiate "QListQSize" (objT c_QSize) mempty

c_QListQSize :: Class
c_QListQSize = c_QList contents_QSize

qmod_QString :: QtModule
qmod_QString = createModule "QString" contents_QString

contents_QString :: Contents
contents_QString = instantiate "QListQString" (objT c_QString) mempty

c_QListQString :: Class
c_QListQString = c_QList contents_QString

qmod_QVariant :: QtModule
qmod_QVariant = createModule "QVariant" contents_QVariant

contents_QVariant :: Contents
contents_QVariant = instantiate "QListQVariant" (objT c_QVariant) mempty

c_QListQVariant :: Class
c_QListQVariant = c_QList contents_QVariant

qmod_QWidget :: QtModule
qmod_QWidget = createModule "QWidget" contents_QWidget

contents_QWidget :: Contents
contents_QWidget = instantiate "QListQWidget" (ptrT $ objT c_QWidget) mempty

c_QListQWidget :: Class
c_QListQWidget = c_QList contents_QWidget
