-- This file is part of Qtah.
--
-- Copyright 2016 Bryan Gardiner <bog@khumba.net>
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

{-# OPTIONS_GHC -W -fwarn-incomplete-patterns -fwarn-unused-do-bind #-}
{-# LANGUAGE CPP #-}

import Control.Monad (when)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  absoluteInstallDirs,
  bindir,
  withPrograms,
  )
import Distribution.Simple.Program (
  Program,
  ProgramSearchPathEntry (ProgramSearchPathDir),
  locationPath,
  lookupProgram,
  programFindLocation,
  programLocation,
  runDbProgram,
  simpleProgram,
  )
import Distribution.Simple.Program.Find (findProgramOnSearchPath)
import Distribution.Simple.Setup (
  CleanFlags,
  CopyDest (NoCopyDest),
  cleanVerbosity,
  fromFlagOrDefault,
  )
import Distribution.Simple.UserHooks (
  UserHooks (
    hookedPrograms,
    cleanHook,
    copyHook,
    instHook,
    postConf
    ),
  )
import Distribution.Simple.Utils (die, info, installExecutableFile)
import Distribution.Verbosity (normal, verbose)
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  getCurrentDirectory,
  removeFile,
  )
import System.FilePath ((</>), joinPath, takeDirectory)

main :: IO ()
main = defaultMainWithHooks qtahHooks

qtahHooks :: UserHooks
qtahHooks = simpleUserHooks
  { hookedPrograms = [listenerGenProgram, qmakeProgram]
  , postConf = \_ _ _ lbi -> generateSources lbi
  , copyHook = \pd lbi uh cf -> do doInstall pd lbi
                                   copyHook simpleUserHooks pd lbi uh cf
  , instHook = \pd lbi uh if' -> do doInstall pd lbi
                                    instHook simpleUserHooks pd lbi uh if'
  , cleanHook = \pd z uh cf -> do doClean cf
                                  cleanHook simpleUserHooks pd z uh cf
  }

listenerGenProgram :: Program
listenerGenProgram =
  (simpleProgram "qtah-listener-gen")
  { programFindLocation = \verbosity _ ->
    findProgramOnSearchPath verbosity [ProgramSearchPathDir "."] "qtah-listener-gen"
  }

qmakeProgram :: Program
qmakeProgram = simpleProgram "qmake"

generateSources :: LocalBuildInfo -> IO ()
generateSources localBuildInfo = do
  let programDb = withPrograms localBuildInfo

  -- Generate binding sources for the generated C++ listener classes.
  runDbProgram normal listenerGenProgram programDb ["--gen-hs-dir", "."]

  -- Generate a Haskell module that provides location of qmake we found.
  qmakeConfiguredProgram <-
    maybe (die "Couldn't find qmake.  Is Qt installed and on the path?") return $
    lookupProgram qmakeProgram programDb
  let qmakeLocation = locationPath $ programLocation qmakeConfiguredProgram
      moduleFile = joinPath ["dist", "build", "autogen", "Graphics", "UI",
                             "Qtah", "Internal", "Generator", "Configure.hs"]
  createDirectoryIfMissing True $ takeDirectory moduleFile
  writeFile moduleFile $ unlines
    [ "---------- GENERATED FILE, EDITS WILL BE LOST ----------"
    , ""
    , "module Graphics.UI.Qtah.Internal.Generator.Configure where"
    , ""
    , "qmakePath :: FilePath"
    , "qmakePath = " ++ show qmakeLocation
    ]

doInstall :: PackageDescription -> LocalBuildInfo -> IO ()
doInstall packageDesc localBuildInfo = do
  startDir <- getCurrentDirectory
  let binDir = bindir $ absoluteInstallDirs packageDesc localBuildInfo NoCopyDest
  createDirectoryIfMissing True binDir
  installExecutableFile verbose
                        (startDir </> "qtah-listener-gen")
                        (binDir </> "qtah-listener-gen")

doClean :: CleanFlags -> IO ()
doClean cleanFlags = do
  startDir <- getCurrentDirectory
  let interfaceDir = startDir </>
                     joinPath ["src", "Graphics", "UI", "Qtah", "Internal", "Interface"]
  delFile $ interfaceDir </> "Listener.hs"
  delFile $ interfaceDir </> "Listener.hs-boot"

  where verbosity = fromFlagOrDefault normal $ cleanVerbosity cleanFlags

        delFile path = do
          exists <- doesFileExist path
          when exists $ do
            info verbosity $ concat ["Removing file ", path, "."]
            removeFile path
