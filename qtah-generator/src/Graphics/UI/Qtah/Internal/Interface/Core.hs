-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Internal.Interface.Core (mods_Core) where

import Foreign.Hoppy.Generator.Spec (Module)
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
  [ (QChar.hoppyModule, QChar.qtModule)
  , (QCoreApplication.hoppyModule, QCoreApplication.qtModule)
  , (QMargins.hoppyModule, QMargins.qtModule)
  , (QObject.hoppyModule, QObject.qtModule)
  , (QPoint.hoppyModule, QPoint.qtModule)
  , (QRect.hoppyModule, QRect.qtModule)
  , (QSize.hoppyModule, QSize.qtModule)
  , (QString.hoppyModule, QString.qtModule)
  , (Types.hoppyModule, Types.qtModule)
  ]
