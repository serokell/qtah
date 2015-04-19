{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QLayout (
  mod_QLayout,
  c_QLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QLayoutItem
import Graphics.UI.Qtah.Internal.Interface.QMargins
import Graphics.UI.Qtah.Internal.Interface.QObject
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QLayout
thisQt = qtc_QLayout
#include "MkQt.hs.inc"

mod_QLayout =
  makeQtModule "QLayout" []
  [ QtExportClass qtc_QLayout
  , QtExportEnum e_SizeConstraint
  ]

c_QLayout = qtClassClass qtc_QLayout

qtc_QLayout =
  makeQtClass (ident "QLayout") Nothing [c_QObject, c_QLayoutItem]
  []
  [ _mkMethod "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , _mkConstMethod "contentsMargins" [] $ TObj c_QMargins
  , _mkMethod "removeWidget" [TPtr $ TObj c_QWidget] TVoid
  , _mkMethod "setContentsMargins" [TObj c_QMargins] TVoid
  , _mkMethod "setSizeConstraint" [TEnum e_SizeConstraint] TVoid
  , _mkMethod "sizeConstraint" [] $ TEnum e_SizeConstraint
  ]
  []

e_SizeConstraint =
  makeEnum (ident1 "QLayout" "SizeConstraint") Nothing
  [ (0, ["set", "default", "size", "constraint"])
  , (1, ["set", "no", "constraint"])
  , (2, ["set", "minimum", "size"])
  , (3, ["set", "fixed", "size"])
  , (4, ["set", "maximum", "size"])
  , (5, ["set", "min", "and", "max", "size"])
  ]
