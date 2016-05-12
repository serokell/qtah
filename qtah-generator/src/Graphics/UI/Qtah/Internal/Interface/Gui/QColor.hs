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

module Graphics.UI.Qtah.Internal.Interface.Gui.QColor (
  aModule,
  c_QColor,
  ) where

import Control.Monad (forM_)
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
  Export (ExportEnum, ExportClass),
  Type (TBool, TEnum, TInt, TObj, TVoid),
  addReqIncludes,
  classSetHaskellConversion,
  hsImports,
  hsQualifiedImport,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_GlobalColor, qreal)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QColor"] $
  collect
  [ just $ QtExport $ ExportClass c_QColor
  , test (qtVersion >= [5, 2]) $ QtExport $ ExportEnum e_NameFormat
  , just $ QtExport $ ExportEnum e_Spec
  ]

-- TODO Everything using QRgb.
c_QColor =
  addReqIncludes [includeStd "QColor"] $
  classSetHaskellConversion conversion $
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "QColor") Nothing []
  [ mkCtor "new" []
  , mkCtor "newRgb" [TInt, TInt, TInt]
  , mkCtor "newRgba" [TInt, TInt, TInt, TInt]
  , mkCtor "newNamedColor" [TObj c_QString]
  , mkCtor "newGlobalColor" [TEnum e_GlobalColor]
  ] $
  collect
  [ just $ mkConstMethod "black" [] TInt
  , just $ mkConstMethod "blackF" [] qreal
  , just $ mkStaticMethod "colorNames" [] $ TObj c_QStringList
  , just $ mkConstMethod "convertTo" [TEnum e_Spec] $ TObj c_QColor
  , just $ mkConstMethod "cyan" [] TInt
  , just $ mkConstMethod "cyanF" [] qreal
  , test (qtVersion >= [4, 3]) $ mkConstMethod' "darker" "darker" [] $ TObj c_QColor
  , test (qtVersion >= [4, 3]) $ mkConstMethod' "darker" "darkerBy" [TInt] $ TObj c_QColor
  , test (qtVersion >= [4, 6]) $ mkConstMethod "hslHue" [] TInt
  , test (qtVersion >= [4, 6]) $ mkConstMethod "hslHueF" [] qreal
  , test (qtVersion >= [4, 6]) $ mkConstMethod "hslSaturation" [] TInt
  , test (qtVersion >= [4, 6]) $ mkConstMethod "hslSaturationF" [] qreal
  , just $ mkConstMethod "hsvHue" [] TInt
  , just $ mkConstMethod "hsvHueF" [] qreal
  , just $ mkConstMethod "hsvSaturation" [] TInt
  , just $ mkConstMethod "hsvSaturationF" [] qreal
  , just $ mkConstMethod "hue" [] TInt
  , just $ mkConstMethod "hueF" [] qreal
  , just $ mkConstMethod "isValid" [] TBool
  , test (qtVersion >= [4, 7]) $ mkStaticMethod "isValidColor" [TObj c_QString] TBool
  , test (qtVersion >= [4, 3]) $ mkConstMethod' "lighter" "lighter" [] $ TObj c_QColor
  , test (qtVersion >= [4, 3]) $ mkConstMethod' "lighter" "lighterBy" [TInt] $ TObj c_QColor
  , test (qtVersion >= [4, 6]) $ mkConstMethod "lightness" [] TInt
  , test (qtVersion >= [4, 6]) $ mkConstMethod "lightnessF" [] qreal
  , just $ mkConstMethod "magenta" [] TInt
  , just $ mkConstMethod "magentaF" [] qreal
  , just $ mkConstMethod' "name" "name" [] $ TObj c_QString
  , test (qtVersion >= [5, 2]) $
    mkConstMethod' "name" "nameWithFormat" [TEnum e_NameFormat] $ TObj c_QString
  , just $ mkConstMethod "saturation" [] TInt
  , just $ mkConstMethod "saturationF" [] qreal
  , just $ mkMethod' "setCmyk" "setCmyk" [TInt, TInt, TInt, TInt] TVoid
  , just $ mkMethod' "setCmyk" "setCmyka" [TInt, TInt, TInt, TInt, TInt] TVoid
  , just $ mkMethod' "setCmykF" "setCmykF" [qreal, qreal, qreal, qreal] TVoid
  , just $ mkMethod' "setCmykF" "setCmykaF" [qreal, qreal, qreal, qreal, qreal] TVoid
  , test (qtVersion >= [4, 6]) $ mkMethod' "setHsl" "setHsl" [TInt, TInt, TInt] TVoid
  , test (qtVersion >= [4, 6]) $ mkMethod' "setHsl" "setHsla" [TInt, TInt, TInt, TInt] TVoid
  , test (qtVersion >= [4, 6]) $ mkMethod' "setHslF" "setHslF" [qreal, qreal, qreal] TVoid
  , test (qtVersion >= [4, 6]) $ mkMethod' "setHslF" "setHslaF" [qreal, qreal, qreal, qreal] TVoid
  , just $ mkMethod' "setHsv" "setHsv" [TInt, TInt, TInt] TVoid
  , just $ mkMethod' "setHsv" "setHsva" [TInt, TInt, TInt, TInt] TVoid
  , just $ mkMethod' "setHsvF" "setHsvF" [qreal, qreal, qreal] TVoid
  , just $ mkMethod' "setHsvF" "setHsvaF" [qreal, qreal, qreal, qreal] TVoid
  , just $ mkMethod "setNamedColor" [TObj c_QString] TVoid
  , just $ mkMethod' "setRgb" "setRgb" [TInt, TInt, TInt] TVoid
  , just $ mkMethod' "setRgb" "setRgba" [TInt, TInt, TInt, TInt] TVoid
  , just $ mkMethod' "setRgbF" "setRgbF" [qreal, qreal, qreal] TVoid
  , just $ mkMethod' "setRgbF" "setRgbaF" [qreal, qreal, qreal, qreal] TVoid
  , just $ mkConstMethod "spec" [] $ TEnum e_Spec
  , just $ mkConstMethod "toCmyk" [] $ TObj c_QColor
  , just $ mkConstMethod "toHsl" [] $ TObj c_QColor
  , just $ mkConstMethod "toHsv" [] $ TObj c_QColor
  , just $ mkConstMethod "toRgb" [] $ TObj c_QColor
  , just $ mkConstMethod "value" [] TInt
  , just $ mkConstMethod "valueF" [] qreal
  , just $ mkConstMethod "yellow" [] TInt
  , just $ mkConstMethod "yellowF" [] qreal
  ] ++
  mkProps
  [ mkProp "alpha" TInt
  , mkProp "alphaF" qreal
  , mkProp "blue" TInt
  , mkProp "blueF" qreal
  , mkProp "green" TInt
  , mkProp "greenF" qreal
  , mkProp "red" TInt
  , mkProp "redF" qreal
  ]

  where
    components =
      [ ("Invalid", "", [])
      , ("Rgb", "rgba", ["red", "green", "blue", "alpha"])
      , ("Cmyk", "cmyka", ["cyan", "magenta", "yellow", "black", "alpha"])
      , ("Hsl", "hsla", ["hslHue", "hslSaturation", "lightness", "alpha"])
      , ("Hsv", "hsva", ["hsvHue", "hsvSaturation", "value", "alpha"])
      ]

    hColorImport = hsQualifiedImport "Graphics.UI.Qtah.Gui.HColor" "HColor"

    conversion =
      ClassHaskellConversion
      { classHaskellConversionType = do
        addImports hColorImport
        return $ HsTyCon $ UnQual $ HsIdent "HColor.HColor"

      , classHaskellConversionToCppFn = do
        addImports $ mconcat [importForPrelude,
                              importForRuntime,
                              hColorImport]
        sayLn "\\color' -> do"
        indent $ do
          sayLn "this' <- qColor_new"
          sayLn "case color' of"
          indent $ forM_ components $ \(spec, letters, _) ->
            saysLn $ concat
            [ ["HColor.", spec]
            , map (\var -> [' ', var, '\'']) letters
            , if null letters
              then [" -> QtahP.return ()"]
              else [" -> qColor_set", spec, "a this'"]
            , concatMap (\var -> [" (QtahFHR.coerceIntegral ", [var], "')"]) letters
            ]
          sayLn "QtahP.return this'"

      , classHaskellConversionFromCppFn = do
        addImports $ mconcat [hsImports "Prelude" ["($)", "(>>=)"],
                              importForPrelude,
                              importForRuntime,
                              hColorImport]
        sayLn "\\this' -> qColor_spec this' >>= \\spec' -> case spec' of"
        indent $ forM_ components $ \(spec, letters, getters) -> do
          saysLn ["QColorSpec_", spec, " -> do"]
          indent $ do
            forM_ (zip letters getters) $ \(var, get) ->
              saysLn [[var], "' <- QtahP.fmap QtahFHR.coerceIntegral $ qColor_", get, " this'"]
            saysLn $ ["QtahP.return $ HColor.", spec] ++ map (\var -> [' ', var, '\'']) letters
      }

-- Introduced in Qt 5.2.
e_NameFormat =
  makeQtEnum (ident1 "QColor" "NameFormat") [includeStd "QColor"]
  [ (0, ["hex", "rgb"])
  , (1, ["hex", "argb"])
  ]

e_Spec =
  makeQtEnum (ident1 "QColor" "Spec") [includeStd "QColor"]
  [ (0, ["invalid"])
  , (1, ["rgb"])
  , (2, ["hsv"])
  , (3, ["cmyk"])
  , (4, ["hsl"])
  ]
