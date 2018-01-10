-- This file is part of Qtah.
--
-- Copyright 2015-2018 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Core.QEvent (
  aModule,
  c_QEvent,
  e_Type,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QEvent"]
  [ QtExportEvent c_QEvent
  , QtExport $ ExportEnum e_Type
  ]

c_QEvent =
  addReqIncludes [includeStd "QEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QEvent") Nothing [] $
  collect
  [ just $ mkCtor "new" [enumT e_Type]
  , just $ mkMethod "accept" [] voidT
  , just $ mkBoolIsProp "accepted"
  , just $ mkMethod "ignore" [] voidT
  , test (qtVersion >= [4, 4]) $
    mkStaticMethod' "registerEventType" "registerEventType" [] intT
  , test (qtVersion >= [4, 4]) $
    mkStaticMethod' "registerEventType" "registerEventTypeWithHint" [intT] intT
  , just $ mkConstMethod "spontaneous" [] boolT
  , just $ mkConstMethod' "type" "eventType" [] $ enumT e_Type  -- 'type' is a Haskell keyword.
  ]

e_Type =
  makeQtEnum (ident1 "QEvent" "Type") [includeStd "QEvent"]
  [ (0, ["none"])
  , (114, ["action", "added"])
  , (113, ["action", "changed"])
  , (115, ["action", "removed"])
  , (99, ["activation", "change"])
  , (121, ["application", "activate"])
    -- ApplicationActivated is also 121, but is omitted here because Hoppy enums
    -- can't handle duplicate numbers.
  , (122, ["application", "deactivate"])
  , (36, ["application", "font", "change"])
  , (37, ["application", "layout", "direction", "change"])
  , (38, ["application", "palette", "change"])
  , (214, ["application", "state", "change"])
  , (35, ["application", "window", "icon", "change"])
  , (68, ["child", "added"])
  , (69, ["child", "polished"])
  , (71, ["child", "removed"])
  , (40, ["clipboard"])
  , (19, ["close"])
  , (200, ["close", "software", "input", "panel"])
  , (178, ["contents", "rect", "change"])
  , (82, ["context", "menu"])
  , (183, ["cursor", "change"])
  , (52, ["deferred", "delete"])
  , (60, ["drag", "enter"])
  , (62, ["drag", "leave"])
  , (61, ["drag", "move"])
  , (63, ["drop"])
  , (170, ["dynamic", "property", "change"])
  , (98, ["enabled", "change"])
  , (10, ["enter"])
  , (150, ["enter", "edit", "focus"])
  , (124, ["enter", "whats", "this", "mode"])
  , (206, ["expose"])
  , (116, ["file", "open"])
  , (8, ["focus", "in"])
  , (9, ["focus", "out"])
  , (23, ["focus", "about", "to", "change"])
  , (97, ["font", "change"])
  , (198, ["gesture"])
  , (202, ["gesture", "override"])
  , (188, ["grab", "keyboard"])
  , (186, ["grab", "mouse"])
  , (159, ["graphics", "scene", "context", "menu"])
  , (164, ["graphics", "scene", "drag", "enter"])
  , (166, ["graphics", "scene", "drag", "leave"])
  , (165, ["graphics", "scene", "drag", "move"])
  , (167, ["graphics", "scene", "drop"])
  , (163, ["graphics", "scene", "help"])
  , (160, ["graphics", "scene", "hover", "enter"])
  , (162, ["graphics", "scene", "hover", "leave"])
  , (161, ["graphics", "scene", "hover", "move"])
  , (158, ["graphics", "scene", "mouse", "double", "click"])
  , (155, ["graphics", "scene", "mouse", "move"])
  , (156, ["graphics", "scene", "mouse", "press"])
  , (157, ["graphics", "scene", "mouse", "release"])
  , (182, ["graphics", "scene", "move"])
  , (181, ["graphics", "scene", "resize"])
  , (168, ["graphics", "scene", "wheel"])
  , (18, ["hide"])
  , (27, ["hide", "to", "parent"])
  , (127, ["hover", "enter"])
  , (128, ["hover", "leave"])
  , (129, ["hover", "move"])
  , (96, ["icon", "drag"])
  , (101, ["icon", "text", "change"])
  , (83, ["input", "method"])
  , (207, ["input", "method", "query"])
  , (169, ["keyboard", "layout", "change"])
  , (6, ["key", "press"])
  , (7, ["key", "release"])
  , (89, ["language", "change"])
  , (90, ["layout", "direction", "change"])
  , (76, ["layout", "request"])
  , (11, ["leave"])
  , (151, ["leave", "edit", "focus"])
  , (125, ["leave", "whats", "this", "mode"])
  , (88, ["locale", "change"])
  , (176, ["non", "client", "area", "mouse", "button", "dbl", "click"])
  , (174, ["non", "client", "area", "mouse", "button", "press"])
  , (175, ["non", "client", "area", "mouse", "button", "release"])
  , (173, ["non", "client", "area", "mouse", "move"])
  , (177, ["mac", "size", "change"])
  , (43, ["meta", "call"])
  , (102, ["modified", "change"])
  , (4, ["mouse", "button", "dbl", "click"])
  , (2, ["mouse", "button", "press"])
  , (3, ["mouse", "button", "release"])
  , (5, ["mouse", "move"])
  , (109, ["mouse", "tracking", "change"])
  , (13, ["move"])
  , (197, ["native", "gesture"])
  , (208, ["orientation", "change"])
  , (12, ["paint"])
  , (39, ["palette", "change"])
  , (131, ["parent", "about", "to", "change"])
  , (21, ["parent", "change"])
  , (212, ["platform", "panel"])
  , (75, ["polish"])
  , (74, ["polish", "request"])
  , (123, ["query", "whats", "this"])
  , (106, ["read", "only", "change"])
  , (199, ["request", "software", "input", "panel"])
  , (14, ["resize"])
  , (204, ["scroll", "prepare"])
  , (205, ["scroll"])
  , (117, ["shortcut"])
  , (51, ["shortcut", "override"])
  , (17, ["show"])
  , (26, ["show", "to", "parent"])
  , (50, ["sock", "act"])
  , (192, ["state", "machine", "signal"])
  , (193, ["state", "machine", "wrapped"])
  , (112, ["status", "tip"])
  , (100, ["style", "change"])
  , (87, ["tablet", "move"])
  , (92, ["tablet", "press"])
  , (93, ["tablet", "release"])
  , (94, ["ok", "request"])
  , (171, ["tablet", "enter", "proximity"])
  , (172, ["tablet", "leave", "proximity"])
  , (22, ["thread", "change"])
  , (1, ["timer"])
  , (120, ["tool", "bar", "change"])
  , (110, ["tool", "tip"])
  , (184, ["tool", "tip", "change"])
  , (194, ["touch", "begin"])
  , (209, ["touch", "cancel"])
  , (196, ["touch", "end"])
  , (195, ["touch", "update"])
  , (189, ["ungrab", "keyboard"])
  , (187, ["ungrab", "mouse"])
  , (78, ["update", "later"])
  , (77, ["update", "request"])
  , (111, ["whats", "this"])
  , (118, ["whats", "this", "clicked"])
  , (31, ["wheel"])
  , (132, ["win", "event", "act"])
  , (24, ["window", "activate"])
  , (103, ["window", "blocked"])
  , (25, ["window", "deactivate"])
  , (34, ["window", "icon", "change"])
  , (105, ["window", "state", "change"])
  , (33, ["window", "title", "change"])
  , (104, ["window", "unblocked"])
  , (203, ["win", "id", "change"])
  , (126, ["z", "order", "change"])
  ]
