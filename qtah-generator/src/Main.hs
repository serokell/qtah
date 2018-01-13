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

module Main where

import Data.List (intercalate)
import Foreign.Hoppy.Generator.Main (run)
import Foreign.Hoppy.Generator.Spec (
  Interface,
  Module,
  interface,
  interfaceAddHaskellModuleBase,
  moduleModify',
  moduleSetCppPath,
  moduleSetHppPath,
  )
import qualified Foreign.Hoppy.Generator.Std as Std
import Graphics.UI.Qtah.Generator.Flags (qmakeArguments, qmakeExecutable, qtVersion)
import Graphics.UI.Qtah.Generator.Module
import qualified Graphics.UI.Qtah.Generator.Interface.Core as Core
import qualified Graphics.UI.Qtah.Generator.Interface.Gui as Gui
import qualified Graphics.UI.Qtah.Generator.Interface.Internal as Internal
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets as Widgets
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (
  dropTrailingPathSeparator,
  takeDirectory,
  takeFileName,
  )

mod_std :: Module
mod_std = moduleModify' Std.mod_std $ do
  moduleSetHppPath "b_std.hpp"
  moduleSetCppPath "b_std.cpp"

modules :: [AModule]
modules =
  concat
  [ [ AHoppyModule mod_std
    ]
  , Core.modules
  , Gui.modules
  , Internal.modules
  , Widgets.modules
  ]

interfaceResult :: Either String Interface
interfaceResult =
  interfaceAddHaskellModuleBase ["Graphics", "UI", "Qtah"] =<<
  interface "qtah" (concatMap aModuleHoppyModules modules)

main :: IO ()
main =
  case interfaceResult of
    Left errorMsg -> do
      putStrLn $ "Error initializing interface: " ++ errorMsg
      exitFailure
    Right iface -> do
      args <- getArgs
      case args of
        ["--qt-version"] -> putStrLn $ intercalate "." $ map show qtVersion
        ["--qmake-executable"] -> putStr $ unlines $ qmakeExecutable : qmakeArguments
        _ -> do
          _ <- run [iface] args
          return ()

findSrcDir :: FilePath -> Maybe FilePath
findSrcDir = go . dropTrailingPathSeparator
  where go "" = Nothing
        go path =
          let dir = takeDirectory path
              file = takeFileName path
          in if file == "src" then Just path
             else if dir == path
                  then Nothing  -- Can't go up any more.
                  else go dir
