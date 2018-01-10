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

-- | General routines for managing events for
-- "Graphics.UI.Qtah.Widgets.QGraphicsScene"s.
module Graphics.UI.Qtah.SceneEvent (
  -- * High-level interface.
  SceneEvent (..),
  SceneEventRegistration,
  unregister,
  -- * Low-level interface
  SceneEventFilter,
  onAnySceneEvent,
  -- * Internal
  internalRegistrationIsLive,
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (when)
import Foreign.Hoppy.Runtime (delete)
import Graphics.UI.Qtah.Widgets.QGraphicsItem (QGraphicsItem, QGraphicsItemPtr)
-- Note, Generated import, since the non-Generated import imports this module.
import Graphics.UI.Qtah.Generated.Core.QEvent (QEvent)
import Graphics.UI.Qtah.Internal.SceneEventListener (SceneEventListener)
import qualified Graphics.UI.Qtah.Internal.SceneEventListener as SceneEventListener

-- | A typeclass for Qt events within a
-- 'Graphics.UI.Qtah.Widgets.QGraphicsScene.QGraphicsScene'.
class SceneEvent e where
  -- | Registers a callback function to be invoked when an event of type @e@ is
  -- sent to an object.  This is a wrapper around 'onAnySceneEvent', so for details,
  -- see that function; all comments about @SceneEventFilter@s apply equally to
  -- handlers given here.
  onSceneEvent :: QGraphicsItemPtr this => this -> (e -> IO Bool) -> IO SceneEventRegistration

-- | A record that an event handler was registered with a receiver object.  This
-- can be given to 'unregister' to destroy the corresponding handler.
data SceneEventRegistration = SceneEventRegistration
  { regListener :: SceneEventListener
  , regLive :: MVar Bool
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
onAnySceneEvent receiver filter = do
  liveVar <- newMVar True
  listener <- SceneEventListener.new receiver filter $ modifyMVar_ liveVar $ const $ return False
  return $ SceneEventRegistration
    { regListener = listener
    , regLive = liveVar
    }

-- | Disconnects an event handler and frees its resources.  This function is
-- idempotent.
unregister :: SceneEventRegistration -> IO ()
unregister reg = modifyMVar_ (regLive reg) $ \live -> do
  when live $ do
    let listener = regListener reg
    -- The on-delete callback tries to modify regLive too, so skip it to avoid
    -- deadlock, because we already know the listener is being deleted.
    SceneEventListener.doNotNotifyOnDelete listener
    delete listener
  return False

internalRegistrationIsLive :: SceneEventRegistration -> IO Bool
internalRegistrationIsLive = readMVar . regLive
