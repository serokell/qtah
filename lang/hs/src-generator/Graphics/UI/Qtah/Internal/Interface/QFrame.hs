{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QFrame (
  c_QFrame,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QFrame
#include "Mk.hs.inc"

c_QFrame =
  makeClass (ident "QFrame") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  [ _mkConstMethod "frameWidth" [] TInt
  , _mkConstMethod "lineWidth" [] TInt
  , _mkConstMethod "midLineWidth" [] TInt
  , _mkConstMethod "setLineWidth" [TInt] TVoid
  , _mkConstMethod "setMidLineWidth" [TInt] TVoid
  ]
