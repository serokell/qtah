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
module Graphics.UI.Qtah.SceneEvent (
  -- * High-level interface.
  SceneEvent (..),
  SceneEventRegistration (..),
  unregister,
  -- * Low-level interface
  SceneEventFilter,
  onAnySceneEvent,
  -- * Internal
  internalOnSceneEvent,
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (when)
import Foreign.C.Types (CInt)
import Foreign.Hoppy.Runtime (delete)
import Foreign.Ptr (Ptr, nullPtr)
-- import Graphics.UI.Qtah.Core.QObject (QObject, QObjectPtr)
import Graphics.UI.Qtah.Generated.Widgets.QGraphicsScene (addItem)
import Graphics.UI.Qtah.Generated.Widgets.QGraphicsItem
  (QGraphicsItem, QGraphicsItemPtr, scene, installSceneEventFilter, removeSceneEventFilter)
-- Note, Generated import, since the non-Generated import imports this module.
import Graphics.UI.Qtah.Generated.Core.QEvent (QEvent)
import Graphics.UI.Qtah.Internal.SceneEventListener (SceneEventListener)
import qualified Graphics.UI.Qtah.Internal.SceneEventListener as SceneEventListener
-- import Graphics.UI.Qtah.Signal (connect)

data Receiver = forall a. QGraphicsItemPtr a => Receiver a

-- | A typeclass for Qt event classes (subclasses of @QEvent@).
class SceneEvent e where
  -- | Registers a callback function to be invoked when an event of type @e@ is
  -- sent to an object.  This is a wrapper around 'onAnySceneEvent', so for details,
  -- see that function; all comments about @SceneEventFilter@s apply equally to
  -- handlers given here.
  onSceneEvent :: QGraphicsItemPtr this => this -> (e -> IO Bool) -> IO SceneEventRegistration

-- | A record that an event handler was registered with a receiver object.  This
-- can be given to 'unregister' to destroy the corresponding handler.
data SceneEventRegistration = SceneEventRegistration
  { regReceiver :: Receiver
  , regListener :: SceneEventListener
  , regActive :: MVar Bool
  }

-- | An filter that can handle any type of event.
type SceneEventFilter = QGraphicsItem -> QEvent -> IO Bool

-- | Registers an 'SceneEventFilter' to listen to events that a 'QGraphicsItem' receives.
-- A filter can return false to allow the event to propagate further, or true to
-- indicate that the event has been handled, and stop propagation.  When
-- multiple filters are attached to an object, the last one installed is called
-- first.  The filter will stay active until the receiver is deleted, or
-- 'unregister' is called.
--
-- This function uses 'QGraphicsItem.installSceneEventFilter' under the hood.
onAnySceneEvent :: QGraphicsItemPtr target =>
  target -> SceneEventFilter -> IO SceneEventRegistration
onAnySceneEvent receiver filter = internalOnSceneEvent receiver nullPtr filter

-- | Internal function, do not use outside of Qtah.
--
-- Implements 'onAnySceneEvent'.  Also takes a pointer to an @int@ that is passed to
-- the underlying 'SceneEventListener.SceneEventListener' object to be set to 1 when the
-- listener is deleted.  This is used for testing purposes.
internalOnSceneEvent :: QGraphicsItemPtr target =>
  target -> Ptr CInt -> SceneEventFilter -> IO SceneEventRegistration
internalOnSceneEvent receiver deletedPtr filter = do
  listener <- SceneEventListener.new filter deletedPtr
  activeVar <- newMVar True
  let reg = SceneEventRegistration
            { regReceiver = Receiver receiver
            , regListener = listener
            , regActive = activeVar
            }
  -- 'addItem' is due to the fact that 'listener' must be on the same scene as 'receiver'.
  scene receiver >>= flip addItem listener
  installSceneEventFilter receiver listener
  -- _ <- connect receiver QGraphicsItem.destroyedSignal $ \_ -> unregister reg
  return reg

-- | Disconnects an event handler and frees its resources.  This function is
-- idempotent.
unregister :: SceneEventRegistration -> IO ()
unregister reg = modifyMVar_ (regActive reg) $ \active -> do
  when active $ do
    let listener = regListener reg
    case regReceiver reg of
      Receiver receiver -> removeSceneEventFilter receiver listener
    delete listener
  return False
