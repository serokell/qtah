-- This file is part of Qtah.
--
-- Copyright 2016 Bryan Gardiner <bog@khumba.net>
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
module Graphics.UI.Qtah.Internal.Interface.Core.QVector (
  -- * Template
  Options (..),
  defaultOptions,
  Contents (..),
  instantiate,
  instantiate',
  toExports,
  -- * Instantiations
  allModules,
  c_QVectorQPoint,
  c_QVectorQPointF,
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
  toHsDataTypeName,
  toHsMethodName',
  )
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Internal.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Internal.Interface.Imports

-- | Options for instantiating the vector classes.
data Options = Options
  { optVectorClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @QVector@ class.  Vectors are always
    -- 'Assignable' and 'Copyable', but you may want to add 'Equatable' if your
    -- value type supports it.
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options []

-- | A set of instantiated classes.
data Contents = Contents
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
        makeClass (identT "QVector" [t]) (Just $ toExtName vectorName) []
        [ mkCtor "new" []
        , mkCtor "newWithSize" [TInt]
        , mkCtor "newWithSizeAndValue" [TInt, t]
        ] $
        collect
        [ just $ mkMethod' "append" "append" [t] TVoid
        , test (qtVersion >= [5, 5]) $ mkMethod' "append" "appendVector" [TObj vector] TVoid
        , just $ mkMethod' OpArray "at" [TInt] $ TRef t
        , just $ mkConstMethod' "at" "atConst" [TInt] $ TRef $ TConst t
        , just $ mkConstMethod "capacity" [] TInt
          -- OMIT back
          -- OMIT begin
          -- OMIT cbegin
          -- OMIT cend
        , just $ mkMethod "clear" [] TVoid
          -- OMIT constBegin
          -- OMIT constData
          -- OMIT constEnd
        , just $ mkConstMethod "contains" [t] TBool
          -- OMIT count()
        , just $ mkConstMethod "count" [t] TInt
        , just $ mkMethod' "data" "array" [] $ TPtr t
        , just $ mkConstMethod' "data" "arrayConst" [] $ TPtr $ TConst t
          -- OMIT empty
        , test (qtVersion >= [4, 5]) $ mkConstMethod "endsWith" [t] TBool
          -- OMIT erase
        , just $ mkMethod' "fill" "fill" [t] TVoid
        , just $ mkMethod' "fill" "fillResize" [t, TInt] TVoid
        , just $ mkMethod' "first" "first" [] $ TRef t
        , just $ mkConstMethod' "first" "firstConst" [] $ TRef $ TConst t
          -- TODO fromList
          -- TODO fromStdVector
          -- OMIT front
        , just $ mkConstMethod' OpArray "get" [TInt] t
        , just $ mkConstMethod' "indexOf" "indexOf" [t] TInt
        , just $ mkConstMethod' "indexOf" "indexOfFrom" [t, TInt] TInt
        , just $ mkMethod' "insert" "insert" [TInt, t] TVoid
        , just $ mkMethod' "insert" "insertMany" [TInt, TInt, t] TVoid
        , just $ mkConstMethod "isEmpty" [] TBool
        , just $ mkMethod' "last" "last" [] $ TRef t
        , just $ mkConstMethod' "last" "lastConst" [] $ TRef $ TConst t
        , just $ mkConstMethod' "lastIndexOf" "lastIndexOf" [t] TInt
        , just $ mkConstMethod' "lastIndexOf" "lastIndexOfFrom" [t, TInt] TInt
          -- OMIT length
        , just $ mkConstMethod' "mid" "mid" [TInt] $ TToGc $ TObj vector
        , just $ mkConstMethod' "mid" "midLength" [TInt, TInt] $ TToGc $ TObj vector
          -- OMIT pop_back
          -- OMIT pop_front
        , just $ mkMethod "prepend" [t] TVoid
          -- OMIT push_back
          -- OMIT push_front
        , just $ mkMethod' "remove" "remove" [TInt] TVoid
        , just $ mkMethod' "remove" "removeMany" [TInt, TInt] TVoid
        , test (qtVersion >= [5, 4]) $ mkMethod "removeAll" [t] TInt
          -- OMIT removeAt
        , test (qtVersion >= [5, 1]) $ mkMethod "removeFirst" [] TVoid
        , test (qtVersion >= [5, 1]) $ mkMethod "removeLast" [] TVoid
        , test (qtVersion >= [5, 4]) $ mkMethod "removeOne" [t] TBool
        , just $ mkMethod "replace" [TInt, t] TVoid
        , just $ mkMethod "reserve" [TInt] TVoid
        , just $ mkMethod "resize" [TInt] TVoid
        , just $ mkConstMethod "size" [] TInt
        , just $ mkMethod "squeeze" [] TVoid
        , test (qtVersion >= [4, 5]) $ mkConstMethod "startsWith" [t] TBool
        , test (qtVersion >= [4, 8]) $ mkMethod "swap" [TRef $ TObj vector] TVoid
        , test (qtVersion >= [5, 2]) $ mkMethod "takeAt" [TInt] t
        , test (qtVersion >= [5, 1]) $ mkMethod "takeFirst" [] t
        , test (qtVersion >= [5, 1]) $ mkMethod "takeLast" [] t
          -- TODO toList
          -- TODO toStdVector
        , just $ mkConstMethod' "value" "value" [TInt] t
        , just $ mkConstMethod' "value" "valueOr" [TInt, t] t
        , just $ mkConstMethod OpAdd [TObj vector] $ TToGc $ TObj vector
        ]

      -- The addendum for the vector class contains HasContents and FromContents
      -- instances.
      addendum = do
        addImports $ mconcat [hsImports "Prelude" ["($)", "(-)"],
                              hsImport1 "Control.Monad" "(<=<)",
                              importForPrelude,
                              importForRuntime]

        forM_ [Const, Nonconst] $ \cst -> do
          let hsDataTypeName = toHsDataTypeName cst vector
          hsValueType <- cppTypeToHsTypeAndUse HsHsSide $ case cst of
            Const -> TConst t
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
              saysLn ["size' <- ", toHsMethodName' vector "size", " this'"]
              saysLn ["QtahP.mapM (QtahFHR.decode <=< ",
                      toHsMethodName' vector vectorAt, " this') [0..size'-1]"]

          -- Only generate a nonconst FromContents instance.
          when (cst == Nonconst) $ do
            ln
            saysLn ["instance QtahFHR.FromContents ", hsDataTypeName,
                    " (", prettyPrint hsValueType, ") where"]
            indent $ do
              sayLn "fromContents values' = do"
              indent $ do
                saysLn ["vector' <- ", toHsMethodName' vector "new"]
                saysLn [toHsMethodName' vector "reserve",
                        " vector' $ QtahFHR.coerceIntegral $ QtahP.length values'"]
                saysLn ["QtahP.mapM_ (", toHsMethodName' vector "append", " vector') values'"]
                sayLn "QtahP.return vector'"

  in Contents
     { c_QVector = vector
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [QtExport]
toExports m = map (QtExport . ExportClass . ($ m)) [c_QVector]

createModule :: String -> Contents -> QtModule
createModule name contents = makeQtModule ["Core", "QVector", name] $ toExports contents

allModules :: [AModule]
allModules =
  map AQtModule
  [ qmod_QPoint
  , qmod_QPointF
  ]

qmod_QPoint :: QtModule
qmod_QPoint = createModule "QPoint" contents_QPoint

contents_QPoint :: Contents
contents_QPoint = instantiate "QVectorQPoint" (TObj c_QPoint) mempty

c_QVectorQPoint :: Class
c_QVectorQPoint = c_QVector contents_QPoint

qmod_QPointF :: QtModule
qmod_QPointF = createModule "QPointF" contents_QPointF

contents_QPointF :: Contents
contents_QPointF = instantiate "QVectorQPointF" (TObj c_QPointF) mempty

c_QVectorQPointF :: Class
c_QVectorQPointF = c_QVector contents_QPointF
