{-# LANGUAGE QuasiQuotes #-}
module Game.TCOD.Mouse(
    mouseShowCursor
  , mouseGetStatus
  , mouseIsCursorVisible
  , mouseMove
  , mouseIncludesTouch
  ) where

import Foreign
import Foreign.C
import Game.TCOD.Context as C
import Game.TCOD.MouseTypes
import Game.TCOD.Noise

context tcodContext
verbatim "#define TCOD_SDL2"
include "mouse.h"

-- | Display and hide the mouse cursor
--
-- By default, the mouse cursor in visible in windowed mode, hidden in
-- fullscreen mode.
mouseShowCursor :: Bool -> IO ()
mouseShowCursor v = do
  let v' = fromBool v
  [C.exp| void {TCOD_mouse_show_cursor($(int v'))}|]

-- | Get the last known mouse cursor position
--
-- This function is only valid, and only returns updated values, after you
-- have called event-related functions.  Whether to check for events, or wait
-- for events.  It does not provide the actual mouse position at the time the
-- call is made.
mouseGetStatus :: IO TCODMouse
mouseGetStatus = alloca $ \res -> do
  [C.exp| void { *$(TCOD_mouse_t* res) = TCOD_mouse_get_status() } |]
  peek res

-- | Getting the cursor status
mouseIsCursorVisible :: IO Bool
mouseIsCursorVisible = toBool <$> [C.exp| int {(int)TCOD_mouse_is_cursor_visible()}|]

-- | Setting the mouse cursor's position
--
-- You can set the cursor position (in pixel coordinates, where [0,0] is the window's top left corner)
mouseMove :: Int -- ^ x
  -> Int -- ^ y
  -> IO ()
mouseMove x y = do
  let x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void { TCOD_mouse_move($(int x'), $(int y'))}|]

-- | Count touches as clicks
mouseIncludesTouch :: Bool -> IO ()
mouseIncludesTouch e = do
  let e' = fromBool e
  [C.exp| void { TCOD_mouse_includes_touch($(int e')!= 0)}|]
