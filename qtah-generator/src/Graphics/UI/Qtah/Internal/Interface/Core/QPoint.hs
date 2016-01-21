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

module Graphics.UI.Qtah.Internal.Interface.Core.QPoint (
  aModule,
  c_QPoint,
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
  ClassConversion (classHaskellConversion),
  ClassHaskellConversion (
      ClassHaskellConversion,
      classHaskellConversionFromCppFn,
      classHaskellConversionToCppFn,
      classHaskellConversionType
  ),
  Export (ExportClass),
  Operator (OpAddAssign, OpDivideAssign, OpMultiplyAssign, OpSubtractAssign),
  Type (TBool, TInt, TObj, TRef),
  addReqIncludes,
  classModifyConversion,
  hsImports,
  hsQualifiedImport,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  mkStaticMethod,
  operatorPreferredExtName',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QPoint"]
  [ QtExport $ ExportClass c_QPoint ]

c_QPoint =
  addReqIncludes [includeStd "QPoint"] $
  classModifyConversion
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"
               return $ HsTyCon $ UnQual $ HsIdent "HPoint.HPoint"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                                     hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint"]
               sayLn "qPoint_new <$> HPoint.x <*> HPoint.y"
             , classHaskellConversionFromCppFn = do
               addImports $ mconcat [hsQualifiedImport "Graphics.UI.Qtah.Core.HPoint" "HPoint",
                                     importForPrelude]
               sayLn "\\q -> do"
               indent $ do
                 sayLn "y <- qPoint_x q"
                 sayLn "x <- qPoint_y q"
                 sayLn "QtahP.return (HPoint.HPoint x y)"
             }
           }) $
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "QPoint") Nothing []
  [ mkCtor "newNull" []
  , mkCtor "new" [TInt, TInt]
  ] $
  collect
  [ test (qtVersion >= [5, 1]) $ mkStaticMethod "dotProduct" [TObj c_QPoint, TObj c_QPoint] TInt
  , just $ mkConstMethod "isNull" [] TBool
  , just $ mkConstMethod "manhattanLength" [] TInt
  , just $ mkMethod OpAddAssign [TObj c_QPoint] $ TRef $ TObj c_QPoint
  , just $ mkMethod OpSubtractAssign [TObj c_QPoint] $ TRef $ TObj c_QPoint
  , just $ mkMethod' OpMultiplyAssign (operatorPreferredExtName' OpMultiplyAssign)
    [TInt] $ TRef $ TObj c_QPoint
  , just $ mkMethod' OpMultiplyAssign (operatorPreferredExtName' OpMultiplyAssign ++ "Real")
    [qreal] $ TRef $ TObj c_QPoint
  , just $ mkMethod OpDivideAssign [qreal] $ TRef $ TObj c_QPoint
  ] ++
  mkProps
  [ mkProp "x" TInt
  , mkProp "y" TInt
  ]
