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
{-# LANGUAGE CPP, RankNTypes #-}

import Control.Monad (unless, when)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  absoluteInstallDirs,
  bindir,
  pkgDescrFile,
  withPrograms,
  )
import Distribution.Simple.Program (Program, runDbProgram, simpleProgram)
import Distribution.Simple.Setup (
  ConfigFlags,
  CleanFlags,
  CopyDest (CopyTo, NoCopyDest),
  cleanVerbosity,
  configVerbosity,
  copyDest,
#if MIN_VERSION_Cabal(2,0,0)
  copyVerbosity,
#endif
  flagToMaybe,
  fromFlagOrDefault,
  installDistPref,
#if MIN_VERSION_Cabal(2,0,0)
  installVerbosity,
#endif
  )
import Distribution.Simple.UserHooks (
  UserHooks (
    cleanHook,
    copyHook,
    hookedPrograms,
    instHook,
    postConf
    ),
  )
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Simple.Utils (die')
#else
import Distribution.Simple.Utils (die)
#endif
import Distribution.Simple.Utils (info, installExecutableFile)
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

type DieFn = forall a. String -> IO a

main :: IO ()
main = defaultMainWithHooks qtahHooks

qtahHooks :: UserHooks
qtahHooks = simpleUserHooks
  { hookedPrograms = [bashProgram]
  , postConf = \_ cf _ lbi -> generateSources cf lbi
  , copyHook = \pd lbi uh cf -> do let dest = fromFlagOrDefault NoCopyDest $ copyDest cf
                                   doInstall pd lbi dest
#if MIN_VERSION_Cabal(2,0,0)
                                     (die' $ fromFlagOrDefault normal $ copyVerbosity cf)
#else
                                     die
#endif

                                   copyHook simpleUserHooks pd lbi uh cf
  , instHook = \pd lbi uh if' -> do let dest = maybe NoCopyDest CopyTo $
                                               flagToMaybe $ installDistPref if'
                                    doInstall pd lbi dest
#if MIN_VERSION_Cabal(2,0,0)
                                      (die' $ fromFlagOrDefault normal $ installVerbosity if')
#else
                                      die
#endif
                                    instHook simpleUserHooks pd lbi uh if'
  , cleanHook = \pd z uh cf -> do doClean cf
                                  cleanHook simpleUserHooks pd z uh cf
  }

bashProgram :: Program
bashProgram = simpleProgram "bash"

findProjectRootDir :: LocalBuildInfo -> DieFn -> IO FilePath
findProjectRootDir localBuildInfo dieFn = case pkgDescrFile localBuildInfo of
  Just path -> return $ takeDirectory path
  Nothing -> dieFn "Couldn't find the project root path."

generateSources :: ConfigFlags -> LocalBuildInfo -> IO ()
generateSources configFlags localBuildInfo = do
  -- Generate binding sources for the generated C++ listener classes.
  let verbosity = fromFlagOrDefault normal $ configVerbosity configFlags

  projectRootDir <-
    findProjectRootDir
    localBuildInfo
#if MIN_VERSION_Cabal(2,0,0)
    (die' verbosity)
#else
    die
#endif

  let genPath = projectRootDir </> "qtah-listener-gen"
      programDb = withPrograms localBuildInfo

  -- Cabal 1.24 (GHC 8?) seems to remove the executable bit from
  -- qtah-listener-gen before configuring, so we have to re-add it.
  --
  -- See: https://github.com/haskell/cabal/issues/4170
  perms <- getPermissions genPath
  unless (executable perms) $
    setPermissions genPath $ setOwnerExecutable True perms

  runDbProgram verbosity bashProgram programDb [genPath, "--gen-hs-dir", "."]

doInstall :: PackageDescription -> LocalBuildInfo -> CopyDest -> DieFn -> IO ()
doInstall packageDesc localBuildInfo copyDest dieFn = do
  projectRootDir <- findProjectRootDir localBuildInfo dieFn
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
