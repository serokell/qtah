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

{-# LANGUAGE ExistentialQuantification #-}

-- | General routines for managing 'QEvent's.
module Graphics.UI.Qtah.Event (
  -- * High-level interface.
  Event (..),
  EventRegistration,
  unregister,
  -- * Low-level interface
  EventFilter,
  onAnyEvent,
  -- * Internal
  internalRegistrationIsLive,
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (when)
import Foreign.Hoppy.Runtime (delete)
-- Importing QObject in both ways is a silly, but it prevents unused import
-- warnings from having a doc link to installEventFilter.
import Graphics.UI.Qtah.Core.QObject (QObjectPtr)
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import Graphics.UI.Qtah.SceneEvent (SceneEvent)
-- Note, Generated import, since the non-Generated import imports this module.
import Graphics.UI.Qtah.Generated.Core.QEvent (QEvent)
import Graphics.UI.Qtah.Internal.EventListener (EventListener)
import qualified Graphics.UI.Qtah.Internal.EventListener as EventListener

-- | A typeclass for Qt event classes (subclasses of @QEvent@).
class SceneEvent e => Event e where
  -- | Registers a callback function to be invoked when an event of type @e@ is
  -- sent to an object.  This is a wrapper around 'onAnyEvent', so for details,
  -- see that function; all comments about @EventFilter@s apply equally to
  -- handlers given here.
  onEvent :: QObjectPtr this => this -> (e -> IO Bool) -> IO EventRegistration

-- | A record that an event handler was registered with a receiver object.  This
-- can be given to 'unregister' to destroy the corresponding handler.
data EventRegistration = EventRegistration
  { regListener :: EventListener
  , regLive :: MVar Bool
  }

-- | An filter that can handle any type of event.
type EventFilter = QObject.QObject -> QEvent -> IO Bool

-- | Registers an 'EventFilter' to listen to events that a 'QObject.QObject' receives.
-- A filter can return false to allow the event to propagate further, or true to
-- indicate that the event has been handled, and stop propagation.  When
-- multiple filters are attached to an object, the last one installed is called
-- first.  The filter will stay active until the receiver is deleted, or
-- 'unregister' is called.
--
-- This function uses 'QObject.installEventFilter' under the hood.
onAnyEvent :: QObjectPtr target => target -> EventFilter -> IO EventRegistration
onAnyEvent receiver filter = do
  liveVar <- newMVar True
  listener <- EventListener.new receiver filter $ modifyMVar_ liveVar $ const $ return False
  return $ EventRegistration
    { regListener = listener
    , regLive = liveVar
    }

-- | Disconnects an event handler and frees its resources.  This function is
-- idempotent.
unregister :: EventRegistration -> IO ()
unregister reg = modifyMVar_ (regLive reg) $ \live -> do
  when live $ do
    let listener = regListener reg
    -- The on-delete callback tries to modify regLive too, so skip it to avoid
    -- deadlock, because we already know the listener is being deleted.
    EventListener.doNotNotifyOnDelete listener
    delete listener
  return False

internalRegistrationIsLive :: EventRegistration -> IO Bool
internalRegistrationIsLive = readMVar . regLive
