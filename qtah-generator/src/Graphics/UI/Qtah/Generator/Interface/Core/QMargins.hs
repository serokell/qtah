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

module Graphics.UI.Qtah.Generator.Interface.Core.QMargins (
  aModule,
  c_QMargins,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
  indent,
  sayLn,
  saysLn,
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
  mkMethod',
  mkProp,
  mkProps,
  operatorPreferredExtName',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
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
  makeQtModule ["Core", "QMargins"]
  [ QtExport $ ExportClass c_QMargins ]

c_QMargins =
  addReqIncludes [includeStd "QMargins"] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = do
      addImports $ hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins"
      return $ HsTyCon $ UnQual $ HsIdent "HMargins.HMargins"
    , classHaskellConversionToCppFn = do
      addImports $ mconcat [hsImports "Control.Applicative" ["(<$>)", "(<*>)"],
                            hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins"]
      saysLn ["qMargins_new <$> HMargins.left <*> HMargins.top <*> HMargins.right <*> ",
              "HMargins.bottom"]
    , classHaskellConversionFromCppFn = do
      addImports $ mconcat [hsQualifiedImport "Graphics.UI.Qtah.Core.HMargins" "HMargins",
                            importForPrelude]
      sayLn "\\q -> do"
      indent $ do
        sayLn "l <- qMargins_left q"
        sayLn "t <- qMargins_top q"
        sayLn "r <- qMargins_right q"
        sayLn "b <- qMargins_bottom q"
        sayLn "QtahP.return (HMargins.HMargins l t r b)"
    } $
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "QMargins") Nothing []
  [ mkCtor "newNull" []
  , mkCtor "new" [intT, intT, intT, intT]
  ] $
  collect
  [ just $ mkConstMethod "isNull" [] boolT
  , test (qtVersion >= [5, 1]) $
    mkMethod' OpAddAssign (operatorPreferredExtName' OpAddAssign)
    [objT c_QMargins] $ refT $ objT c_QMargins
  , test (qtVersion >= [5, 1]) $
    mkMethod' OpAddAssign (operatorPreferredExtName' OpAddAssign ++ "Int")
    [intT] $ refT $ objT c_QMargins
  , test (qtVersion >= [5, 1]) $
    mkMethod' OpSubtractAssign (operatorPreferredExtName' OpSubtractAssign)
    [objT c_QMargins] $ refT $ objT c_QMargins
  , test (qtVersion >= [5, 1]) $
    mkMethod' OpSubtractAssign (operatorPreferredExtName' OpSubtractAssign ++ "Int")
    [intT] $ refT $ objT c_QMargins
  , test (qtVersion >= [5, 1]) $
    mkMethod' OpMultiplyAssign (operatorPreferredExtName' OpMultiplyAssign)
    [intT] $ refT $ objT c_QMargins
  , test (qtVersion >= [5, 1]) $
    mkMethod' OpMultiplyAssign (operatorPreferredExtName' OpMultiplyAssign ++ "Real")
    [qreal] $ refT $ objT c_QMargins
  , test (qtVersion >= [5, 1]) $
    mkMethod' OpDivideAssign (operatorPreferredExtName' OpDivideAssign)
    [intT] $ refT $ objT c_QMargins
  , test (qtVersion >= [5, 1]) $
    mkMethod' OpDivideAssign (operatorPreferredExtName' OpDivideAssign ++ "Real")
    [qreal] $ refT $ objT c_QMargins
  ] ++
  mkProps
  [ mkProp "bottom" intT
  , mkProp "left" intT
  , mkProp "right" intT
  , mkProp "top" intT
  ]
