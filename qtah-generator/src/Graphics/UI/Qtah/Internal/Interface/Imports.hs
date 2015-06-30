-- | Shared bindings for @\"Qtah*\"@ qualified imports in generated bindings.
-- Some classes use qualified imports that don't start with @\"Qtah\"@; such
-- imports are one-off and are not listed here.
module Graphics.UI.Qtah.Internal.Interface.Imports (
  importForChar,
  importForForeign,
  importForForeignC,
  importForPrelude,
  importForSignal,
  importForSupport,
  ) where

import Foreign.Cppop.Generator.Spec (HsImportSet, hsQualifiedImport)

importForChar :: HsImportSet
importForChar = hsQualifiedImport "Data.Char" "QtahDC"

importForForeign :: HsImportSet
importForForeign = hsQualifiedImport "Foreign" "QtahF"

importForForeignC :: HsImportSet
importForForeignC = hsQualifiedImport "Foreign.C" "QtahFC"

importForPrelude :: HsImportSet
importForPrelude = hsQualifiedImport "Prelude" "QtahP"

importForSignal :: HsImportSet
importForSignal = hsQualifiedImport "Graphics.UI.Qtah.Signal" "QtahSignal"

importForSupport :: HsImportSet
importForSupport = hsQualifiedImport "Foreign.Cppop.Runtime.Support" "QtahFCRS"
