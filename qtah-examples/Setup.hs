-- This file is part of Qtah.
--
-- Copyright 2016-2017 Bryan Gardiner <bog@khumba.net>
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

import Control.Monad (unless)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup (ConfigFlags, configDynExe, configVerbosity, fromFlagOrDefault)
import Distribution.Simple.UserHooks (UserHooks (postConf))
import Distribution.Simple.Utils (warn)
import Distribution.Verbosity (normal)

main :: IO ()
main = defaultMainWithHooks qtahHooks

qtahHooks :: UserHooks
qtahHooks = simpleUserHooks
  { postConf = \_ cf _ _ -> warnAboutDynExe cf
  }

warnAboutDynExe :: ConfigFlags -> IO ()
warnAboutDynExe configFlags = do
  let verbosity = fromFlagOrDefault normal $ configVerbosity configFlags
  unless (fromFlagOrDefault False $ configDynExe configFlags) $
    warn verbosity $
    "qtah-examples needs to be a dynamically linked executable; " ++
    "please pass --enable-executable-dynamic to 'cabal install'."
