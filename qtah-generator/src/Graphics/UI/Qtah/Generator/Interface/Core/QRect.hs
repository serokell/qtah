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

module Graphics.UI.Qtah.Generator.Interface.Core.QRect (
  aModule,
  c_QRect,
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
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  hsImports,
  hsQualifiedImport,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
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
  makeQtModule ["Core", "QRect"]
  [ QtExport $ ExportClass c_QRect ]

c_QRect =
  addReqIncludes [includeStd "QRect"] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HRect" "HRect"
      return $ HsTyCon $ UnQual $ HsIdent "HRect.HRect"
    , classHaskellConversionToCppFn = Just $ do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HRect" "HRect"]
      sayLn "newWithRaw <$> HRect.x <*> HRect.y <*> HRect.width <*> HRect.height"
    , classHaskellConversionFromCppFn = Just $ do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HRect" "HRect"]
      sayLn "\\q -> HRect.HRect <$> x q <*> y q <*> width q <*> height q"
    } $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QRect") Nothing [] $
  collect
  [ just $ mkCtor "newNull" []
  , just $ mkCtor "newWithPoints" [objT c_QPoint, objT c_QPoint]
  , just $ mkCtor "newWithPointAndSize" [objT c_QPoint, objT c_QSize]
  , just $ mkCtor "newWithRaw" [intT, intT, intT, intT]
  , just $ mkMethod "adjust" [intT, intT, intT, intT] voidT
  , just $ mkConstMethod "adjusted" [intT, intT, intT, intT] $ objT c_QRect
  , just $ mkProp "bottom" intT
  , just $ mkProp "bottomLeft" $ objT c_QPoint
  , just $ mkProp "bottomRight" $ objT c_QPoint
  , just $ mkConstMethod "center" [] $ objT c_QPoint
  , just $ mkConstMethod' "contains" "containsPoint" [objT c_QPoint, boolT] boolT
  , just $ mkConstMethod' "contains" "containsRect" [objT c_QRect, boolT] boolT
  , just $ mkProp "height" intT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "intersected" [objT c_QRect] $ objT c_QRect
  , just $ mkConstMethod "intersects" [objT c_QRect] boolT
  , just $ mkConstMethod "isEmpty" [] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkProp "left" intT
  , test (qtVersion >= [5, 1]) $ mkConstMethod "marginsAdded" [objT c_QMargins] $ objT c_QRect
  , test (qtVersion >= [5, 1]) $ mkConstMethod "marginsRemoved" [objT c_QMargins] $ objT c_QRect
  , just $ mkMethod "moveBottom" [intT] voidT
  , just $ mkMethod "moveBottomLeft" [objT c_QPoint] voidT
  , just $ mkMethod "moveBottomRight" [objT c_QPoint] voidT
  , just $ mkMethod "moveCenter" [objT c_QPoint] voidT
  , just $ mkMethod "moveLeft" [intT] voidT
  , just $ mkMethod "moveRight" [intT] voidT
  , just $ mkMethod "moveTo" [objT c_QPoint] voidT
  , just $ mkMethod "moveTop" [intT] voidT
  , just $ mkMethod "moveTopLeft" [objT c_QPoint] voidT
  , just $ mkMethod "moveTopRight" [objT c_QPoint] voidT
  , just $ mkConstMethod "normalized" [] $ objT c_QRect
  , just $ mkProp "right" intT
  , just $ mkMethod "setCoords" [intT, intT, intT, intT] voidT
  , just $ mkMethod "setRect" [intT, intT, intT, intT] voidT
  , just $ mkProp "size" $ objT c_QSize
  , just $ mkProp "top" intT
  , just $ mkProp "topLeft" $ objT c_QPoint
  , just $ mkProp "topRight" $ objT c_QPoint
  , just $ mkMethod "translate" [objT c_QPoint] voidT
  , just $ mkConstMethod "translated" [objT c_QPoint] $ objT c_QRect
  , test (qtVersion >= [4, 2]) $ mkMethod "united" [objT c_QRect] $ objT c_QRect
  , just $ mkProp "width" intT
  , just $ mkProp "x" intT
  , just $ mkProp "y" intT
  ]
