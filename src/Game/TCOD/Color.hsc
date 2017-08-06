{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
module Game.TCOD.Color(
    Color(..)
  ) where

import Game.TCOD.Context

import Foreign

context tcodContext
#include "color.h" -- hsc2hs
include "color.h" -- inline-c

instance Storable Color where
  sizeOf    _ = #{size TCOD_color_t}
  alignment _ = #{alignment TCOD_color_t}
  poke p Color{..} = do
    #{poke TCOD_color_t, r} p colorR
    #{poke TCOD_color_t, g} p colorG
    #{poke TCOD_color_t, b} p colorB
  peek p = Color
    <$> (#{peek TCOD_color_t, r} p)
    <*> (#{peek TCOD_color_t, g} p)
    <*> (#{peek TCOD_color_t, b} p)
