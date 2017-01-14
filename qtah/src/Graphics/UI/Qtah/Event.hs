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
  internalOnEvent,
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (when)
import Foreign.C.Types (CInt)
import Foreign.Hoppy.Runtime (delete)
import Foreign.Ptr (Ptr, nullPtr)
import Graphics.UI.Qtah.Core.QObject (QObject, QObjectPtr)
import qualified Graphics.UI.Qtah.Core.QObject as QObject
-- Note, Generated import, since the non-Generated import imports this module.
import Graphics.UI.Qtah.Generated.Core.QEvent (QEvent)
import Graphics.UI.Qtah.Internal.EventListener (EventListener)
import qualified Graphics.UI.Qtah.Internal.EventListener as EventListener
import Graphics.UI.Qtah.Signal (connect)

data Receiver = forall a. QObjectPtr a => Receiver a

-- | A typeclass for Qt event classes (subclasses of @QEvent@).
class Event e where
  -- | Registers a callback function to be invoked when an event of type @e@ is
  -- sent to an object.  This is a wrapper around 'onAnyEvent', so for details,
  -- see that function; all comments about @EventFilter@s apply equally to
  -- handlers given here.
  onEvent :: QObjectPtr this => this -> (e -> IO Bool) -> IO EventRegistration

-- | A record that an event handler was registered with a receiver object.  This
-- can be given to 'unregister' to destroy the corresponding handler.
data EventRegistration = EventRegistration
  { regReceiver :: Receiver
  , regListener :: EventListener
  , regActive :: MVar Bool
  }

-- | An filter that can handle any type of event.
type EventFilter = QObject -> QEvent -> IO Bool

-- | Registers an 'EventFilter' to listen to events that a 'QObject' receives.
-- A filter can return false to allow the event to propagate further, or true to
-- indicate that the event has been handled, and stop propagation.  When
-- multiple filters are attached to an object, the last one installed is called
-- first.  The filter will stay active until the receiver is deleted, or
-- 'unregister' is called.
--
-- This function uses 'QObject.installEventFilter' under the hood.
onAnyEvent :: QObjectPtr target => target -> EventFilter -> IO EventRegistration
onAnyEvent receiver filter = internalOnEvent receiver nullPtr filter

-- | Internal function, do not use outside of Qtah.
--
-- Implements 'onAnyEvent'.  Also takes a pointer to an @int@ that is passed to
-- the underlying 'EventListener.EventListener' object to be set to 1 when the
-- listener is deleted.  This is used for testing purposes.
internalOnEvent :: QObjectPtr target => target -> Ptr CInt -> EventFilter -> IO EventRegistration
internalOnEvent receiver deletedPtr filter = do
  listener <- EventListener.new filter deletedPtr
  activeVar <- newMVar True
  let reg = EventRegistration
            { regReceiver = Receiver receiver
            , regListener = listener
            , regActive = activeVar
            }
  QObject.installEventFilter receiver listener
  _ <- connect receiver QObject.destroyedSignal $ \_ -> unregister reg
  return reg

-- | Disconnects an event handler and frees its resources.  This function is
-- idempotent.
unregister :: EventRegistration -> IO ()
unregister reg = modifyMVar_ (regActive reg) $ \active -> do
  when active $ do
    let listener = regListener reg
    case regReceiver reg of
      Receiver receiver -> QObject.removeEventFilter receiver listener
    delete listener
  return False
