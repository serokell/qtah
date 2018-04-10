-- This file is part of Qtah.
--
-- Copyright 2018 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Core.QSettings (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  )
import Foreign.Hoppy.Generator.Types (objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Core", "QSettings"]
  [QtExport $ ExportClass c_QSettings]

c_QSettings :: Class
c_QSettings =
  addReqIncludes [includeStd "QSettings"] $
  classSetEntityPrefix "" $
  makeClass (ident "QSettings") Nothing [c_QObject]
  [ mkCtor "new" []
  , mkCtor "newWithOrganization" [objT c_QString]
  , mkCtor "newWithOrganizationAndApplication" [objT c_QString, objT c_QString]
  , mkCtor "newWithOrganizationAndApplicationAndParent"
    [objT c_QString, objT c_QString, ptrT $ objT c_QObject]
  , mkCtor "newWithParent" [ptrT $ objT c_QObject]
  -- TODO QSettings(Scope scope, const QString &organization,
  --      const QString &application = QString(), QObject *parent = Q_NULLPTR)
  -- TODO QSettings(Format format, Scope scope, const QString &organization,
  --      const QString &application = QString(), QObject *parent = Q_NULLPTR)
  -- TODO QSettings(const QString &fileName, Format format, QObject *parent = Q_NULLPTR)
  , mkMethod "setValue" [objT c_QString, objT c_QVariant] voidT
  , mkConstMethod "value" [objT c_QString] (objT c_QVariant)
  , mkConstMethod' "value" "valueWithDefault" [objT c_QString, objT c_QVariant] (objT c_QVariant)
  -- TODO Other methods.
  ]
