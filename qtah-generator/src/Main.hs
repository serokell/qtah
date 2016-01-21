-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

import Data.Foldable (forM_)
import Foreign.Hoppy.Generator.Main (Action (GenHaskell), run)
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
import Graphics.UI.Qtah.Internal.Generator.Common (maybeFail)
import Graphics.UI.Qtah.Internal.Generator.Module
import Graphics.UI.Qtah.Internal.Generator.Types
import qualified Graphics.UI.Qtah.Internal.Interface.Callback as Callback
import qualified Graphics.UI.Qtah.Internal.Interface.Core as Core
import qualified Graphics.UI.Qtah.Internal.Interface.Gui as Gui
import qualified Graphics.UI.Qtah.Internal.Interface.Listener as Listener
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets as Widgets
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
    , Callback.aModule
    , Listener.aModule
    ]
  , Core.modules
  , Gui.modules
  , Widgets.modules
  ]

interfaceResult :: Either String Interface
interfaceResult =
  interfaceAddHaskellModuleBase ["Graphics", "UI", "Qtah", "Generated"] =<<
  interface "qtah" (map aModuleHoppy modules)

main :: IO ()
main =
  case interfaceResult of
    Left errorMsg -> do
      putStrLn $ "Error initializing interface: " ++ errorMsg
      exitFailure
    Right iface -> do
      args <- getArgs
      actions <- run [iface] args
      forM_ actions $ \action -> case action of
        GenHaskell path -> do
          -- Generate nicely-named Qt modules that will point to the bindings,
          -- and also contain signal definitions.
          srcDir <- maybeFail ("Couldn't find src directory for path " ++ show path ++
                               " to generate Qt modules.") $
                    findSrcDir path
          forM_ modules $ \aModule -> case aModule of
            AHoppyModule _ -> return ()
            AQtModule qm -> generateModule iface srcDir "Graphics.UI.Qtah" qm

        _ -> return ()

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
