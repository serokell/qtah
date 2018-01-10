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

{-# OPTIONS_GHC -W -fwarn-incomplete-patterns -fwarn-unused-do-bind #-}

import Control.Monad (unless, when)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  absoluteInstallDirs,
  bindir,
  pkgDescrFile,
  )
import Distribution.Simple.Program (runProgram)
import Distribution.Simple.Program.Types (
  ProgramLocation (FoundOnSystem),
  simpleConfiguredProgram,
  )
import Distribution.Simple.Setup (
  CleanFlags,
  CopyDest (CopyTo, NoCopyDest),
  cleanVerbosity,
  copyDest,
  flagToMaybe,
  fromFlagOrDefault,
  installDistPref,
  )
import Distribution.Simple.UserHooks (
  UserHooks (
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
  executable,
  getCurrentDirectory,
  getPermissions,
  removeFile,
  setOwnerExecutable,
  setPermissions,
  )
import System.FilePath ((</>), joinPath, takeDirectory)

main :: IO ()
main = defaultMainWithHooks qtahHooks

qtahHooks :: UserHooks
qtahHooks = simpleUserHooks
  { postConf = \_ _ _ lbi -> generateSources lbi
  , copyHook = \pd lbi uh cf -> do let dest = fromFlagOrDefault NoCopyDest $ copyDest cf
                                   doInstall pd lbi dest
                                   copyHook simpleUserHooks pd lbi uh cf
  , instHook = \pd lbi uh if' -> do let dest = maybe NoCopyDest CopyTo $
                                               flagToMaybe $ installDistPref if'
                                    doInstall pd lbi dest
                                    instHook simpleUserHooks pd lbi uh if'
  , cleanHook = \pd z uh cf -> do doClean cf
                                  cleanHook simpleUserHooks pd z uh cf
  }

findProjectRootDir :: LocalBuildInfo -> IO FilePath
findProjectRootDir localBuildInfo = case pkgDescrFile localBuildInfo of
  Just path -> return $ takeDirectory path
  Nothing -> die "Couldn't find the project root path."

generateSources :: LocalBuildInfo -> IO ()
generateSources localBuildInfo = do
  -- Generate binding sources for the generated C++ listener classes.
  projectRootDir <- findProjectRootDir localBuildInfo
  let genPath = projectRootDir </> "qtah-listener-gen"
      program = simpleConfiguredProgram "qtah-listener-gen" $ FoundOnSystem genPath

  -- Cabal 1.24 (GHC 8?) seems to remove the executable bit from
  -- qtah-listener-gen before configuring, so we have to re-add it.
  perms <- getPermissions genPath
  unless (executable perms) $
    setPermissions genPath $ setOwnerExecutable True perms

  runProgram normal program ["--gen-hs-dir", "."]

doInstall :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
doInstall packageDesc localBuildInfo copyDest = do
  projectRootDir <- findProjectRootDir localBuildInfo
  let binDir = bindir $ absoluteInstallDirs packageDesc localBuildInfo copyDest
  createDirectoryIfMissing True binDir
  installExecutableFile verbose
                        (projectRootDir </> "qtah-listener-gen")
                        (binDir </> "qtah-listener-gen")

doClean :: CleanFlags -> IO ()
doClean cleanFlags = do
  startDir <- getCurrentDirectory
  let interfaceDir = startDir </>
                     joinPath ["src", "Graphics", "UI", "Qtah", "Generator", "Interface"]
  delFile $ interfaceDir </> "Listener.hs"
  delFile $ interfaceDir </> "Listener.hs-boot"

  where verbosity = fromFlagOrDefault normal $ cleanVerbosity cleanFlags

        delFile path = do
          exists <- doesFileExist path
          when exists $ do
            info verbosity $ concat ["Removing file ", path, "."]
            removeFile path
