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

{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Generator.Interface.Core.QTextCodec (
  aModule,
  c_QTextCodec,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  classSetDtorPrivate,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListInt, c_QListQByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QTextCodec"]
  [ QtExport $ ExportClass c_QTextCodec
  ]

c_QTextCodec =
  addReqIncludes [includeStd "QTextCodec"] $
  classSetDtorPrivate $
  classSetEntityPrefix "" $
  makeClass (ident "QTextCodec") Nothing [] $
  collect
  [ just $ mkConstMethod "aliases" [] $ objT c_QListQByteArray
  , just $ mkConstMethod' "canEncode" "canEncodeChar" [objT c_QChar] boolT
  , just $ mkConstMethod' "canEncode" "canEncodeString" [objT c_QString] boolT
  , just $ mkConstMethod "fromUnicode" [objT c_QString] $ objT c_QByteArray
    -- TODO QByteArray fromUnicode(const QChar*, int, ConverterState* =) const
    -- TODO QTextDecoder* makeDecoder(ConversionFlags =) const
    -- TODO QTextEncoder* makeEncoder(ConversionFlags =) const
  , just $ mkConstMethod "mibEnum" [] intT
  , just $ mkConstMethod "name" [] $ objT c_QByteArray
  , just $ mkConstMethod "toUnicode" [objT c_QByteArray] $ objT c_QString
    -- TODO QString toUnicode(const char*) const
    -- TODO QString toUnicode(const char*, int, ConverterState* =) const

  , just $ mkStaticMethod "availableCodecs" [] $ objT c_QListQByteArray
  , just $ mkStaticMethod "availableMibs" [] $ objT c_QListInt
  , just $ mkStaticMethod' "codecForHtml" "codecForHtml"
    [objT c_QByteArray] $ ptrT $ objT c_QTextCodec
  , just $ mkStaticMethod' "codecForHtml" "codecForHtmlWithDefault"
    [objT c_QByteArray, ptrT $ objT c_QTextCodec] $ ptrT $ objT c_QTextCodec
  , just $ mkStaticMethod "codecForLocale" [] $ ptrT $ objT c_QTextCodec
  , just $ mkStaticMethod "codecForMib" [intT] $ ptrT $ objT c_QTextCodec
  , just $ mkStaticMethod "codecForName" [objT c_QByteArray] $ ptrT $ objT c_QTextCodec
    -- TODO QTextCodec* codecForName(const char*)  (Omit this?)
  , just $ mkStaticMethod' "codecForUtfText" "codecForUtfText"
    [objT c_QByteArray] $ ptrT $ objT c_QTextCodec
  , just $ mkStaticMethod' "codecForUtfText" "codecForUtfTextWithDefault"
    [objT c_QByteArray, ptrT $ objT c_QTextCodec] $ ptrT $ objT c_QTextCodec
  , just $ mkStaticMethod "setCodecForLocale" [ptrT $ objT c_QTextCodec] voidT
  ]

-- TODO ConverterState, ConversionFlag, ConversionFlags
