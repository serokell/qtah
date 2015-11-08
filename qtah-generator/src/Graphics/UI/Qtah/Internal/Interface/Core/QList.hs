-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}

-- | Bindings for @QList@.
module Graphics.UI.Qtah.Internal.Interface.Core.QList (
  -- * Template
  Options (..),
  defaultOptions,
  Contents (..),
  instantiate,
  instantiate',
  toExports,
  -- * Instantiations
  allModules,
  c_QListQString,
  c_QListQWidget,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, when)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat, mempty)
#endif
import Foreign.Hoppy.Generator.Language.Haskell.General (
  HsTypeSide (HsHsSide),
  addImports,
  cppTypeToHsTypeAndUse,
  indent,
  ln,
  prettyPrint,
  sayLn,
  saysLn,
  toHsDataTypeName,
  toHsMethodName',
  )
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Std (ValueConversion (ConvertPtr, ConvertValue))
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

-- | Options for instantiating the list classes.
data Options = Options
  { optListClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @QList@ class.  Lists are always
    -- 'Assignable' and 'Copyable', but you may want to add 'Equatable' if your
    -- value type supports it.
  , optValueConversion :: Maybe ValueConversion
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options [] Nothing

-- | A set of instantiated classes.
data Contents = Contents
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
                     , reqInclude $ includeLocal "shim_qlist.hpp"
                     ]
      features = Assignable : Copyable : optListClassFeatures opts
      conversion = optValueConversion opts
      isValueConvertible = case conversion of
        Nothing -> False
        Just ConvertPtr -> False
        Just ConvertValue -> True
      hasReserve = qtVersion >= [4, 7]

      list =
        (case conversion of
           Nothing -> id
           Just conversion -> addAddendumHaskell $ makeAddendum conversion) $
        addUseReqs reqs $
        classAddFeatures features $
        makeClass (identT "QList" [t]) (Just $ toExtName listName) []
        [ mkCtor "new" []
        ] $
        collect
        [ just $ mkMethod' "append" "append" [t] TVoid
        , test (qtVersion >= [4, 5]) $ mkMethod' "append" "appendList" [TObj list] TVoid
        , just $ mkMethod' OpArray "at" [TInt] $ TRef t
        , just $ mkConstMethod' "at" "atConst" [TInt] $ TRef $ TConst t
          -- OMIT begin
        , just $ mkMethod "clear" [] TVoid
        , just $ mkConstMethod "contains" [t] TBool
          -- OMIT count()
        , just $ mkConstMethod "count" [t] TInt
        , test (qtVersion >= [4, 5]) $ mkConstMethod "endsWith" [t] TBool
          -- OMIT erase
        , just $ mkMethod' "first" "first" [] $ TRef t
        , just $ mkConstMethod' "first" "firstConst" [] $ TRef $ TConst t
        , test isValueConvertible $ mkConstMethod' OpArray "get" [TInt] t
        , just $ mkConstMethod' "indexOf" "indexOf" [t] TInt
        , just $ mkConstMethod' "indexOf" "indexOfFrom" [t, TInt] TInt
        , just $ mkMethod "insert" [TInt, t] TVoid
        , just $ mkConstMethod "isEmpty" [] TBool
        , just $ mkMethod' "last" "last" [] $ TRef t
        , just $ mkConstMethod' "last" "lastConst" [] $ TRef $ TConst t
        , just $ mkConstMethod' "lastIndexOf" "lastIndexOf" [t] TInt
        , just $ mkConstMethod' "lastIndexOf" "lastIndexOfFrom" [t, TInt] TInt
          -- OMIT length
        , just $ mkConstMethod' "mid" "newSubseq" [TInt] $ TObjToHeap list
        , just $ mkConstMethod' "mid" "newSubseqLength" [TInt, TInt] $ TObjToHeap list
        , just $ mkMethod "move" [TInt, TInt] TVoid
        , just $ mkMethod "prepend" [t] TVoid
        , just $ makeFnMethod (ident2 "qtah" "qlist" "put") "put" MNormal Nonpure
          [TRef $ TObj list, TInt, t] TVoid
        , just $ mkMethod "removeAll" [t] TInt
        , just $ mkMethod "removeAt" [TInt] TVoid
        , just $ mkMethod "removeFirst" [] TVoid
        , just $ mkMethod "removeLast" [] TVoid
        , test (qtVersion >= [4, 4]) $ mkMethod "removeOne" [t] TBool
        , just $ mkMethod "replace" [TInt, t] TVoid
        , test hasReserve $ mkMethod "reserve" [TInt] TVoid
        , just $ mkConstMethod "size" [] TInt
        , test (qtVersion >= [4, 5]) $ mkConstMethod "startsWith" [t] TBool
        , just $ mkMethod "swap" [TInt, TInt] TVoid
          -- OMIT swap(QList<T>&)
        , test isValueConvertible $ mkMethod "takeAt" [TInt] t
        , test isValueConvertible $ mkMethod "takeFirst" [] t
        , test isValueConvertible $ mkMethod "takeLast" [] t
          -- TODO toSet
          -- TODO toStdList
          -- TODO toVector
        , test isValueConvertible $ mkConstMethod' "value" "value" [TInt] t
        , test isValueConvertible $ mkConstMethod' "value" "valueOr" [TInt, t] t
          -- OMIT operator+ because it creates a new object quietly (would need
          -- TObjToHeap).
        ]

      -- The addendum for the list class contains HasContents and FromContents
      -- instances.
      makeAddendum conversion = do
        addImports $ mconcat [hsImport1 "Prelude" "(-)",
                              importForPrelude,
                              importForSupport]
        when (conversion == ConvertValue) $
          addImports $ hsImport1 "Control.Monad" "(<=<)"
        when hasReserve $
          addImports $ hsImport1 "Prelude" "($)"

        forM_ [Const, Nonconst] $ \cst -> do
          let hsDataTypeName = toHsDataTypeName cst list
          hsValueType <-
            cppTypeToHsTypeAndUse HsHsSide $
            (case conversion of
               ConvertPtr -> TPtr
               ConvertValue -> id) $
            case cst of
              Const -> TConst t
              Nonconst -> t

          -- Generate const and nonconst HasContents instances.
          ln
          saysLn ["instance QtahFHRS.HasContents ", hsDataTypeName,
                  " (", prettyPrint hsValueType, ") where"]
          indent $ do
            sayLn "toContents this' = do"
            indent $ do
              let listAt = case cst of
                    Const -> "atConst"
                    Nonconst -> "at"
              saysLn ["size' <- ", toHsMethodName' list "size", " this'"]
              saysLn ["QtahP.mapM (",
                      case conversion of
                        ConvertPtr -> ""
                        ConvertValue -> "QtahFHRS.decode <=< ",
                      toHsMethodName' list listAt, " this') [0..size'-1]"]

          -- Only generate a nonconst FromContents instance.
          when (cst == Nonconst) $ do
            ln
            saysLn ["instance QtahFHRS.FromContents ", hsDataTypeName,
                    " (", prettyPrint hsValueType, ") where"]
            indent $ do
              sayLn "fromContents values' = do"
              indent $ do
                saysLn ["list' <- ", toHsMethodName' list "new"]
                when hasReserve $
                  saysLn [toHsMethodName' list "reserve",
                          " list' $ QtahFHRS.coerceIntegral $ QtahP.length values'"]
                saysLn ["QtahP.mapM_ (", toHsMethodName' list "append", " list') values'"]
                sayLn "QtahP.return list'"

  in Contents
     { c_QList = list
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [QtExport]
toExports m = map (QtExport . ExportClass . ($ m)) [c_QList]

createModule :: String -> Contents -> QtModule
createModule name contents = makeQtModule ["Core", "QList", name] $ toExports contents

allModules :: [AModule]
allModules =
  map AQtModule
  [ qmod_QString
  , qmod_QWidget
  ]

qmod_QString :: QtModule
qmod_QString = createModule "QString" contents_QString

contents_QString :: Contents
contents_QString =
  instantiate' "QListQString" (TObj c_QString) mempty $
  defaultOptions { optValueConversion = Just ConvertValue }

c_QListQString :: Class
c_QListQString = c_QList contents_QString

qmod_QWidget :: QtModule
qmod_QWidget = createModule "QWidget" contents_QWidget

contents_QWidget :: Contents
contents_QWidget =
  instantiate' "QListQWidget" (TPtr $ TObj c_QWidget) mempty $
  defaultOptions { optValueConversion = Just ConvertValue }

c_QListQWidget :: Class
c_QListQWidget = c_QList contents_QWidget
