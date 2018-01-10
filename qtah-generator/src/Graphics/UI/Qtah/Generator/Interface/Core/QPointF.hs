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

module Graphics.UI.Qtah.Generator.Interface.Core.QPointF (
  aModule,
  c_QPointF,
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
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
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
  makeQtModule ["Core", "QPointF"]
  [ QtExport $ ExportClass c_QPointF ]

c_QPointF =
  addReqIncludes [includeStd "QPointF"] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HPointF" "HPointF"
      return $ HsTyCon $ UnQual $ HsIdent "HPointF.HPointF"
    , classHaskellConversionToCppFn = Just $ do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HPointF" "HPointF"]
      sayLn "new <$> HPointF.x <*> HPointF.y"
    , classHaskellConversionFromCppFn = Just $ do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HPointF" "HPointF"]
      sayLn "\\q -> HPointF.HPointF <$> x q <*> y q"
    } $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QPointF") Nothing [] $
  collect
  [ just $ mkCtor "newNull" []
  , just $ mkCtor "new" [qreal, qreal]
  , just $ mkCtor "newFromPoint" [objT c_QPoint]
  , test (qtVersion >= [5, 1]) $ mkStaticMethod "dotProduct" [objT c_QPointF, objT c_QPointF] qreal
  , just $ mkConstMethod "isNull" [] boolT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "manhattanLength" [] qreal
  , just $ mkConstMethod "toPoint" [] $ objT c_QPoint
  , just $ mkProp "x" qreal
  , just $ mkProp "y" qreal
  , just $ mkMethod OpAddAssign [objT c_QPointF] $ refT $ objT c_QPointF
  , just $ mkMethod OpSubtractAssign [objT c_QPointF] $ refT $ objT c_QPointF
  , just $ mkMethod OpMultiplyAssign [qreal] $ refT $ objT c_QPointF
  , just $ mkMethod OpDivideAssign [qreal] $ refT $ objT c_QPointF
  ]
