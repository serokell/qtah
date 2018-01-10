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

module Graphics.UI.Qtah.Generator.Interface.Core.QSize (
  aModule,
  c_QSize,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  ClassHaskellConversion (
    ClassHaskellConversion,
    classHaskellConversionFromCppFn,
    classHaskellConversionToCppFn,
    classHaskellConversionType
  ),
  Export (ExportClass),
  Operator (OpAddAssign, OpDivideAssign, OpMultiplyAssign, OpSubtractAssign),
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  hsImports,
  hsQualifiedImport,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_AspectRatioMode, qreal)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QSize"]
  [ QtExport $ ExportClass c_QSize ]

c_QSize =
  addReqIncludes [includeStd "QSize"] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"
      return $ HsTyCon $ UnQual $ HsIdent "HSize.HSize"
    , classHaskellConversionToCppFn = Just $ do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"]
      sayLn "new <$> HSize.width <*> HSize.height"
    , classHaskellConversionFromCppFn = Just $ do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"]
      sayLn "\\q -> HSize.HSize <$> width q <*> height q"
    } $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QSize") Nothing [] $
  collect
  [ just $ mkCtor "newNull" []
  , just $ mkCtor "new" [intT, intT]
  , just $ mkConstMethod "boundedTo" [objT c_QSize] $ objT c_QSize
  , just $ mkConstMethod "expandedTo" [objT c_QSize] $ objT c_QSize
  , just $ mkProp "height" intT
  , just $ mkConstMethod "isEmpty" [] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkMethod "scale" [objT c_QSize, enumT e_AspectRatioMode] voidT
  , test (qtVersion >= [5, 0]) $
    mkConstMethod "scaled" [objT c_QSize, enumT e_AspectRatioMode] $ objT c_QSize
  , just $ mkMethod "transpose" [] voidT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "transposed" [] $ objT c_QSize
  , just $ mkProp "width" intT
  , just $ mkMethod OpAddAssign [objT c_QSize] $ refT $ objT c_QSize
  , just $ mkMethod OpSubtractAssign [objT c_QSize] $ refT $ objT c_QSize
  , just $ mkMethod OpMultiplyAssign [qreal] $ refT $ objT c_QSize
  , just $ mkMethod OpDivideAssign [qreal] $ refT $ objT c_QSize
  ]
