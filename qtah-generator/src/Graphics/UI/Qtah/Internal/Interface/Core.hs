module Graphics.UI.Qtah.Internal.Interface.Core (mods_Core) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QChar as QChar
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QMargins as QMargins
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QPoint as QPoint
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QRect as QRect
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QSize as QSize
import qualified Graphics.UI.Qtah.Internal.Interface.Core.QString as QString
import qualified Graphics.UI.Qtah.Internal.Interface.Core.Types as Types

{-# ANN module "HLint: ignore Use camelCase" #-}

mods_Core :: [(Module, QtModule)]
mods_Core =
  [ (QChar.cppopModule, QChar.qtModule)
  , (QCoreApplication.cppopModule, QCoreApplication.qtModule)
  , (QMargins.cppopModule, QMargins.qtModule)
  , (QObject.cppopModule, QObject.qtModule)
  , (QPoint.cppopModule, QPoint.qtModule)
  , (QRect.cppopModule, QRect.qtModule)
  , (QSize.cppopModule, QSize.qtModule)
  , (QString.cppopModule, QString.qtModule)
  , (Types.cppopModule, Types.qtModule)
  ]
