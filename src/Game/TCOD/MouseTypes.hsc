{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
module Game.TCOD.MouseTypes(
    TCODMouse(..)
  , defaultTCODMouse
  ) where

import Foreign
import Game.TCOD.Context
import GHC.Generics

#include "libtcod/mouse_types.h"

-- | Get 'TCODMouse' with default values
defaultTCODMouse :: TCODMouse
defaultTCODMouse = TCODMouse {
    mouseX = 0
  , mouseY = 0
  , mouseDx = 0
  , mouseDy = 0
  , mouseCx = 0
  , mouseCy = 0
  , mouseDcx = 0
  , mouseDcy = 0
  , mouseLButton = False
  , mouseRButton = False
  , mouseMButton = False
  , mouseLButtonPressed = False
  , mouseRButtonPressed = False
  , mouseMButtonPressed = False
  , mouseWheelUp = False
  , mouseWheelDown = False
  }

instance Storable TCODMouse where
  sizeOf _ = #{size TCOD_mouse_t}
  alignment _ = #{alignment TCOD_mouse_t}
  poke p TCODMouse{..} = do
    #{poke TCOD_mouse_t, x} p mouseX
    #{poke TCOD_mouse_t, y} p mouseY
    #{poke TCOD_mouse_t, dx} p mouseDx
    #{poke TCOD_mouse_t, dy} p mouseDy
    #{poke TCOD_mouse_t, cx} p mouseCx
    #{poke TCOD_mouse_t, cy} p mouseCy
    #{poke TCOD_mouse_t, dcx} p mouseDcx
    #{poke TCOD_mouse_t, dcy} p mouseDcy
    #{poke TCOD_mouse_t, lbutton} p mouseLButton
    #{poke TCOD_mouse_t, rbutton} p mouseRButton
    #{poke TCOD_mouse_t, mbutton} p mouseMButton
    #{poke TCOD_mouse_t, lbutton_pressed} p mouseLButtonPressed
    #{poke TCOD_mouse_t, rbutton_pressed} p mouseRButtonPressed
    #{poke TCOD_mouse_t, mbutton_pressed} p mouseMButtonPressed
    #{poke TCOD_mouse_t, wheel_up} p mouseWheelUp
    #{poke TCOD_mouse_t, wheel_down} p mouseWheelDown
  peek p = TCODMouse
    <$> (#{peek TCOD_mouse_t, x} p)
    <*> (#{peek TCOD_mouse_t, y} p)
    <*> (#{peek TCOD_mouse_t, dx} p)
    <*> (#{peek TCOD_mouse_t, dy} p)
    <*> (#{peek TCOD_mouse_t, cx} p)
    <*> (#{peek TCOD_mouse_t, cy} p)
    <*> (#{peek TCOD_mouse_t, dcx} p)
    <*> (#{peek TCOD_mouse_t, dcy} p)
    <*> (#{peek TCOD_mouse_t, lbutton} p)
    <*> (#{peek TCOD_mouse_t, rbutton} p)
    <*> (#{peek TCOD_mouse_t, mbutton} p)
    <*> (#{peek TCOD_mouse_t, lbutton_pressed} p)
    <*> (#{peek TCOD_mouse_t, rbutton_pressed} p)
    <*> (#{peek TCOD_mouse_t, mbutton_pressed} p)
    <*> (#{peek TCOD_mouse_t, wheel_up} p)
    <*> (#{peek TCOD_mouse_t, wheel_down} p)
