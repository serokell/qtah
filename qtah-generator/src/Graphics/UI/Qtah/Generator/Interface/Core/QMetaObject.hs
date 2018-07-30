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

module Graphics.UI.Qtah.Generator.Interface.Core.QMetaObject (
  aModule,
  c_QMetaObject,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  MethodApplicability (MConst),
  Purity (Nonpure),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, constT, intT, objT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaClassInfo (c_QMetaClassInfo)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaEnum (c_QMetaEnum)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaMethod (c_QMetaMethod)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaProperty (c_QMetaProperty)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QMetaObject"]
  [ QtExport $ ExportClass c_QMetaObject ]

c_QMetaObject =
  addReqIncludes [ includeStd "QMetaObject"
                 , includeLocal "wrap_qmetaobject.hpp"
                 ] $
  classSetConversionToGc $
  classAddFeatures [Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QMetaObject") Nothing []
  [ mkConstMethod "classInfo" [intT] $ objT c_QMetaClassInfo
  , mkConstMethod "classInfoCount" [] intT
  , mkConstMethod "classInfoOffset" [] intT
  , mkConstMethod "constructor" [intT] $ objT c_QMetaMethod
  , mkConstMethod "constructorCount" [] intT
  , mkConstMethod "enumerator" [intT] $ objT c_QMetaEnum
  , mkConstMethod "enumeratorCount" [] intT
  , mkConstMethod "enumeratorOffset" [] intT
  , makeFnMethod (ident2 "qtah" "qmetaobject" "indexOfClassInfo") "indexOfClassInfo"
    MConst Nonpure [objT c_QMetaObject, objT c_QString] intT
  , makeFnMethod (ident2 "qtah" "qmetaobject" "indexOfConstructor") "indexOfConstructor"
    MConst Nonpure [objT c_QMetaObject, objT c_QString] intT
  , makeFnMethod (ident2 "qtah" "qmetaobject" "indexOfEnumerator") "indexOfEnumerator"
    MConst Nonpure [objT c_QMetaObject, objT c_QString] intT
  , makeFnMethod (ident2 "qtah" "qmetaobject" "indexOfMethod") "indexOfMethod"
    MConst Nonpure [objT c_QMetaObject, objT c_QString] intT
  , makeFnMethod (ident2 "qtah" "qmetaobject" "indexOfProperty") "indexOfProperty"
    MConst Nonpure [objT c_QMetaObject, objT c_QString] intT
  , makeFnMethod (ident2 "qtah" "qmetaobject" "indexOfSignal") "indexOfSignal"
    MConst Nonpure [objT c_QMetaObject, objT c_QString] intT
  , makeFnMethod (ident2 "qtah" "qmetaobject" "indexOfSlot") "indexOfSlot"
    MConst Nonpure [objT c_QMetaObject, objT c_QString] intT
  , mkConstMethod "inherits" [ptrT $ constT $ objT c_QMetaObject] boolT
  , mkConstMethod "method" [intT] $ objT c_QMetaMethod
  , mkConstMethod "methodCount" [] intT
  , mkConstMethod "methodOffset" [] intT
    -- TODO newInstance
  , mkConstMethod "property" [intT] $ objT c_QMetaProperty
  , mkConstMethod "propertyCount" [] intT
  , mkConstMethod "propertyOffset" [] intT
  , mkConstMethod "superClass" [] $ ptrT $ constT $ objT c_QMetaObject
  , mkConstMethod "userProperty" [] $ objT c_QMetaProperty

    -- TODO Static methods
  ]
