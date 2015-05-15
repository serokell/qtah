module Graphics.UI.Qtah.Internal.Interface.Core (mod_Core, qmods_Core) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QMargins as QMargins
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QPoint as QPoint
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QRect as QRect
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QSize as QSize
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QString as QString

{-# ANN module "HLint: ignore Use camelCase" #-}

mod_Core :: Module
mod_Core = modifyModule' (makeModule "core" "core.hpp" "core.cpp") $
  addModuleExports $ concatMap qtModuleExports qmods_Core

qmods_Core :: [QtModule]
qmods_Core =
  [ QCoreApplication.qtModule
  , QMargins.qtModule
  , QObject.qtModule
  , QPoint.qtModule
  , QRect.qtModule
  , QSize.qtModule
  , QString.qtModule
  ]
