module Graphics.UI.Qtah.Internal.Interface.Core.QString (
  cppopModule,
  qtModule,
  c_QString,
  ) where

import Data.Monoid (mconcat)
import Foreign.Cppop.Generator.Language.Haskell.General (
  addImports,
  sayLn,
  )
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Spec.ClassFeature
import Foreign.Cppop.Generator.Std.String (c_string)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Core" "QString" qtModule

qtModule =
  makeQtModule "Core.QString"
  [ QtExport $ ExportClass c_QString
  , QtExportFnRenamed f_QString_set "set"
  ]

this = c_QString

c_QString =
  addReqIncludes [includeStd "QString"] $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports importForPrelude
               return $ HsTyCon $ UnQual $ HsIdent "QtahP.String"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [importForForeignC, importForPrelude]
               sayLn "QtahP.flip QtahFC.withCString qString_newFromCString"
             , classHaskellConversionFromCppFn = sayLn "qString_toStdString"
             }
           }) $
  makeClass (ident "QString") Nothing []
  [ mkCtor this "newFromCString" [TPtr $ TConst TChar]
  ]
  [ mkConstMethod' this OpArray "at" [TInt] $ TObj c_QChar
  , mkConstMethod this "toStdString" [] $ TObj c_string
  ]

f_QString_set =
  addReqIncludes [includeLocal "shim_qstring.hpp"] $
  makeFn (ident1 "qtah" "shim_QString_set") (Just $ toExtName "QString_set")
  Nonpure [TRef $ TObj c_QString, TInt, TObj c_QChar] TVoid
