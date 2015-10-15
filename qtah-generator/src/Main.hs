module Main where

import Control.Arrow (second)
import Data.Foldable (forM_)
import Foreign.Hoppy.Common (maybeFail)
import Foreign.Hoppy.Generator.Main (Action (GenHaskell), run)
import Foreign.Hoppy.Generator.Spec (
  Interface,
  Module,
  interface,
  interfaceAddHaskellModuleBase,
  modifyModule',
  setModuleCppPath,
  setModuleHppPath,
  )
import qualified Foreign.Hoppy.Generator.Std as Std
import Graphics.UI.Qtah.Internal.Generator.Module
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Callback (mod_Callback, qmods_Callback)
import Graphics.UI.Qtah.Internal.Interface.Core (mods_Core)
import Graphics.UI.Qtah.Internal.Interface.Listener (mod_Listener, qmods_Listener)
import Graphics.UI.Qtah.Internal.Interface.Widgets (mods_Widgets)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (
  dropTrailingPathSeparator,
  takeDirectory,
  takeFileName,
  )

mod_std :: Module
mod_std = modifyModule' Std.mod_std $ do
  setModuleHppPath "b_std.hpp"
  setModuleCppPath "b_std.cpp"

modules :: [(Module, [QtModule])]
modules =
  [ (mod_std, [])
  , (mod_Callback, qmods_Callback)
  , (mod_Listener, qmods_Listener)
  ] ++ map (second (:[])) (mods_Core ++ mods_Widgets)

interfaceResult :: Either String Interface
interfaceResult =
  interfaceAddHaskellModuleBase ["Graphics", "UI", "Qtah", "Generated"] =<<
  interface "qtah" (map fst modules)

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
          forM_ modules $ \(_, qtModules) ->
            forM_ qtModules $ \qm ->
            generateModule iface srcDir "Graphics.UI.Qtah" qm

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
