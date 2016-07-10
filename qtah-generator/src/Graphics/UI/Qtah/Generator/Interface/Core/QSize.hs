-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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
  indent,
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
  mkProps,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_AspectRatioMode, qreal)
import Graphics.UI.Qtah.Generator.Interface.Imports
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
    { classHaskellConversionType = do
      addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"
      return $ HsTyCon $ UnQual $ HsIdent "HSize.HSize"
    , classHaskellConversionToCppFn = do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize"]
      sayLn "qSize_new <$> HSize.width <*> HSize.height"
    , classHaskellConversionFromCppFn = do
      addImports $ mconcat [hsQualifiedImport "Graphics.UI.Qtah.Core.HSize" "HSize",
                            importForPrelude]
      sayLn "\\q -> do"
      indent $ do
        sayLn "w <- qSize_width q"
        sayLn "h <- qSize_height q"
        sayLn "QtahP.return (HSize.HSize w h)"
    } $
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "QSize") Nothing []
  [ mkCtor "newNull" []
  , mkCtor "new" [intT, intT]
  ] $
  collect
  [ just $ mkConstMethod "boundedTo" [objT c_QSize] $ objT c_QSize
  , just $ mkConstMethod "expandedTo" [objT c_QSize] $ objT c_QSize
  , just $ mkConstMethod "isEmpty" [] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkMethod "scale" [objT c_QSize, enumT e_AspectRatioMode] voidT
  , test (qtVersion >= [5, 0]) $
    mkConstMethod "scaled" [objT c_QSize, enumT e_AspectRatioMode] $ objT c_QSize
  , just $ mkMethod "transpose" [] voidT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "transposed" [] $ objT c_QSize
  , just $ mkMethod OpAddAssign [objT c_QSize] $ refT $ objT c_QSize
  , just $ mkMethod OpSubtractAssign [objT c_QSize] $ refT $ objT c_QSize
  , just $ mkMethod OpMultiplyAssign [qreal] $ refT $ objT c_QSize
  , just $ mkMethod OpDivideAssign [qreal] $ refT $ objT c_QSize
  ] ++
  mkProps
  [ mkProp "height" intT
  , mkProp "width" intT
  ]
