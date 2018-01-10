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

-- | Bindings for @QVector@.
module Graphics.UI.Qtah.Generator.Interface.Core.QVector (
  -- * Template
  Options (..),
  defaultOptions,
  Contents (..),
  -- * Instantiations
  allModules,
  c_QVectorInt,
  c_QVectorQPoint,
  c_QVectorQPointF,
  c_QVectorQRgb,
  ) where

import Control.Monad (forM_, when)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat, mempty)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  HsTypeSide (HsHsSide),
  addImports,
  cppTypeToHsTypeAndUse,
  indent,
  ln,
  prettyPrint,
  sayLn,
  saysLn,
  toHsClassEntityName',
  toHsDataTypeName,
  )
import Foreign.Hoppy.Generator.Spec (
  Class,
  Constness (Const, Nonconst),
  Export (ExportClass),
  Operator (OpAdd, OpArray),
  Reqs,
  Type,
  addReqs,
  addAddendumHaskell,
  classSetEntityPrefix,
  classSetMonomorphicSuperclass,
  hsImport1,
  hsImports,
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
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (qrgb)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), QtModule, makeQtModule)
import Graphics.UI.Qtah.Generator.Types

-- | Options for instantiating the vector classes.
newtype Options = Options
  { optVectorClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @QVector@ class.  Vectors are always
    -- 'Assignable' and 'Copyable', but you may want to add 'Equatable' if your
    -- value type supports it.
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options []

-- | A set of instantiated classes.
newtype Contents = Contents
  { c_QVector :: Class  -- ^ @QVector\<T>@
  }

-- | @instantiate className t tReqs@ creates a set of bindings for an
-- instantiation of @QVector@ and associated types (e.g. iterators).  In the
-- result, the 'c_QVector' class has an external name of @className@.
instantiate :: String -> Type -> Reqs -> Contents
instantiate vectorName t tReqs = instantiate' vectorName t tReqs defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Reqs -> Options -> Contents
instantiate' vectorName t tReqs opts =
  let reqs = mconcat [ tReqs
                     , reqInclude $ includeStd "QVector"
                     ]
      features = Assignable : Copyable : optVectorClassFeatures opts

      vector =
        addReqs reqs $
        addAddendumHaskell addendum $
        classAddFeatures features $
        classSetMonomorphicSuperclass $
        classSetEntityPrefix "" $
        makeClass (identT "QVector" [t]) (Just $ toExtName vectorName) [] $
        collect
        [ just $ mkCtor "new" []
        , just $ mkCtor "newWithSize" [intT]
        , just $ mkCtor "newWithSizeAndValue" [intT, t]
        , just $ mkMethod' "append" "append" [t] voidT
        , test (qtVersion >= [5, 5]) $ mkMethod' "append" "appendVector" [objT vector] voidT
        , just $ mkMethod' OpArray "at" [intT] $ refT t
        , just $ mkConstMethod' "at" "atConst" [intT] $ refT $ constT t
        , just $ mkConstMethod "capacity" [] intT
          -- OMIT back
          -- OMIT begin
          -- OMIT cbegin
          -- OMIT cend
        , just $ mkMethod "clear" [] voidT
          -- OMIT constBegin
          -- OMIT constData
          -- OMIT constEnd
        , just $ mkConstMethod "contains" [t] boolT
          -- OMIT count()
        , just $ mkConstMethod "count" [t] intT
        , just $ mkMethod' "data" "array" [] $ ptrT t
        , just $ mkConstMethod' "data" "arrayConst" [] $ ptrT $ constT t
          -- OMIT empty
        , test (qtVersion >= [4, 5]) $ mkConstMethod "endsWith" [t] boolT
          -- OMIT erase
        , just $ mkMethod' "fill" "fill" [t] voidT
        , just $ mkMethod' "fill" "fillResize" [t, intT] voidT
        , just $ mkMethod' "first" "first" [] $ refT t
        , just $ mkConstMethod' "first" "firstConst" [] $ refT $ constT t
          -- TODO fromList
          -- TODO fromStdVector
          -- OMIT front
        , just $ mkConstMethod' OpArray "get" [intT] t
        , just $ mkConstMethod' "indexOf" "indexOf" [t] intT
        , just $ mkConstMethod' "indexOf" "indexOfFrom" [t, intT] intT
        , just $ mkMethod' "insert" "insert" [intT, t] voidT
        , just $ mkMethod' "insert" "insertMany" [intT, intT, t] voidT
        , just $ mkConstMethod "isEmpty" [] boolT
        , just $ mkMethod' "last" "last" [] $ refT t
        , just $ mkConstMethod' "last" "lastConst" [] $ refT $ constT t
        , just $ mkConstMethod' "lastIndexOf" "lastIndexOf" [t] intT
        , just $ mkConstMethod' "lastIndexOf" "lastIndexOfFrom" [t, intT] intT
          -- OMIT length
        , just $ mkConstMethod' "mid" "mid" [intT] $ toGcT $ objT vector
        , just $ mkConstMethod' "mid" "midLength" [intT, intT] $ toGcT $ objT vector
          -- OMIT pop_back
          -- OMIT pop_front
        , just $ mkMethod "prepend" [t] voidT
          -- OMIT push_back
          -- OMIT push_front
        , just $ mkMethod' "remove" "remove" [intT] voidT
        , just $ mkMethod' "remove" "removeMany" [intT, intT] voidT
        , test (qtVersion >= [5, 4]) $ mkMethod "removeAll" [t] intT
          -- OMIT removeAt
        , test (qtVersion >= [5, 1]) $ mkMethod "removeFirst" [] voidT
        , test (qtVersion >= [5, 1]) $ mkMethod "removeLast" [] voidT
        , test (qtVersion >= [5, 4]) $ mkMethod "removeOne" [t] boolT
        , just $ mkMethod "replace" [intT, t] voidT
        , just $ mkMethod "reserve" [intT] voidT
        , just $ mkMethod "resize" [intT] voidT
        , just $ mkConstMethod "size" [] intT
        , just $ mkMethod "squeeze" [] voidT
        , test (qtVersion >= [4, 5]) $ mkConstMethod "startsWith" [t] boolT
        , test (qtVersion >= [4, 8]) $ mkMethod "swap" [refT $ objT vector] voidT
        , test (qtVersion >= [5, 2]) $ mkMethod "takeAt" [intT] t
        , test (qtVersion >= [5, 1]) $ mkMethod "takeFirst" [] t
        , test (qtVersion >= [5, 1]) $ mkMethod "takeLast" [] t
          -- TODO toList
          -- TODO toStdVector
        , just $ mkConstMethod' "value" "value" [intT] t
        , just $ mkConstMethod' "value" "valueOr" [intT, t] t
        , just $ mkConstMethod OpAdd [objT vector] $ toGcT $ objT vector
        ]

      -- The addendum for the vector class contains HasContents and FromContents
      -- instances.
      addendum = do
        addImports $ mconcat [hsImports "Prelude" ["($)", "(-)"],
                              hsImport1 "Control.Monad" "(<=<)",
                              importForPrelude,
                              importForRuntime]

        forM_ [Const, Nonconst] $ \cst -> do
          hsDataTypeName <- toHsDataTypeName cst vector
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
              let vectorAt = case cst of
                    Const -> "atConst"
                    Nonconst -> "at"
              saysLn ["size' <- ", toHsClassEntityName' vector "size", " this'"]
              saysLn ["QtahP.mapM (QtahFHR.decode <=< ",
                      toHsClassEntityName' vector vectorAt, " this') [0..size'-1]"]

          -- Only generate a nonconst FromContents instance.
          when (cst == Nonconst) $ do
            ln
            saysLn ["instance QtahFHR.FromContents ", hsDataTypeName,
                    " (", prettyPrint hsValueType, ") where"]
            indent $ do
              sayLn "fromContents values' = do"
              indent $ do
                saysLn ["vector' <- ", toHsClassEntityName' vector "new"]
                saysLn [toHsClassEntityName' vector "reserve",
                        " vector' $ QtahFHR.coerceIntegral $ QtahP.length values'"]
                saysLn ["QtahP.mapM_ (", toHsClassEntityName' vector "append", " vector') values'"]
                sayLn "QtahP.return vector'"

  in Contents
     { c_QVector = vector
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [QtExport]
toExports m = [QtExport $ ExportClass $ c_QVector m]

createModule :: String -> Contents -> QtModule
createModule name contents = makeQtModule ["Core", "QVector", name] $ toExports contents

allModules :: [AModule]
allModules =
  map AQtModule
  [ qmod_Int
  , qmod_QPoint
  , qmod_QPointF
  , qmod_QRgb
  ]

qmod_Int :: QtModule
qmod_Int = createModule "Int" contents_Int

contents_Int :: Contents
contents_Int = instantiate "QVectorInt" intT mempty

c_QVectorInt :: Class
c_QVectorInt = c_QVector contents_Int

qmod_QPoint :: QtModule
qmod_QPoint = createModule "QPoint" contents_QPoint

contents_QPoint :: Contents
contents_QPoint = instantiate "QVectorQPoint" (objT c_QPoint) mempty

c_QVectorQPoint :: Class
c_QVectorQPoint = c_QVector contents_QPoint

qmod_QPointF :: QtModule
qmod_QPointF = createModule "QPointF" contents_QPointF

contents_QPointF :: Contents
contents_QPointF = instantiate "QVectorQPointF" (objT c_QPointF) mempty

c_QVectorQPointF :: Class
c_QVectorQPointF = c_QVector contents_QPointF

qmod_QRgb :: QtModule
qmod_QRgb = createModule "QRgb" contents_QRgb

contents_QRgb :: Contents
contents_QRgb = instantiate "QVectorQRgb" qrgb mempty

c_QVectorQRgb :: Class
c_QVectorQRgb = c_QVector contents_QRgb
