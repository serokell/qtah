{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QFrame (
  mod_QFrame,
  c_QFrame,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QFrame
thisQt = qtc_QFrame
#include "MkQt.hs.inc"

mod_QFrame =
  makeQtModule "QFrame" []
  [ QtExportClass thisQt ]

c_QFrame = qtClassClass qtc_QFrame

qtc_QFrame =
  makeQtClass (ident "QFrame") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  [ _mkConstMethod "frameWidth" [] TInt
  , _mkConstMethod "lineWidth" [] TInt
  , _mkConstMethod "midLineWidth" [] TInt
  , _mkConstMethod "setLineWidth" [TInt] TVoid
  , _mkConstMethod "setMidLineWidth" [TInt] TVoid
  ]
  []
