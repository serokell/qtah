{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame (
  qtModule,
  c_QFrame,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

qtModule =
  makeQtModule "Widgets.QFrame"
  [ QtExport $ ExportClass c_QFrame
  , QtExport $ ExportEnum e_Shadow
  , QtExport $ ExportEnum e_Shape
  , QtExport $ ExportEnum e_StyleMask
  ]

this = c_QFrame

c_QFrame =
  addReqIncludes [includeStd "QFrame"] $
  makeClass (ident "QFrame") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
    -- TODO QFrame(QWidget*, Qt::WindowFlags)
  ] $
  [ _mkConstMethod "frameWidth" [] TInt
  ] ++
  _props
  [ _mkProp "frameRect" $ TObj c_QRect
  , _mkProp "frameShadow" $ TEnum e_Shadow
  , _mkProp "frameShape" $ TEnum e_Shape
  , _mkProp "frameStyle" TInt
  , _mkProp "lineWidth" TInt
  , _mkProp "midLineWidth" TInt
  ]

e_Shadow =
  makeEnum (ident1 "QFrame" "Shadow") Nothing
  [ (0x0010, ["plain"])
  , (0x0020, ["raised"])
  , (0x0030, ["sunken"])
  ]

e_Shape =
  makeEnum (ident1 "QFrame" "Shape") Nothing
  [ (0x0000, ["no", "frame"])
  , (0x0001, ["box"])
  , (0x0002, ["panel"])
  , (0x0003, ["win", "panel"])
  , (0x0004, ["h", "line"])
  , (0x0005, ["v", "line"])
  , (0x0006, ["styled", "panel"])
  ]

e_StyleMask =
  makeEnum (ident1 "QFrame" "StyleMask") Nothing
  [ (0x000f, ["shape", "mask"])
  , (0x00f0, ["shadow", "mask"])
  ]
