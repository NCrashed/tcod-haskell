{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Game.TCOD.HeightmapInst(

  ) where

import Game.TCOD.Context
import Data.Array.Repa (extent, Z(..), (:.)(..))
import Data.Array.Repa.Repr.ForeignPtr
import Foreign

#include "heightmap.h"

instance Storable TCODHeightMap where
  sizeOf _ = #{size TCOD_heightmap_t}
  alignment _ = #{alignment TCOD_heightmap_t}
  poke p (TCODHeightMap arr) = withForeignPtr arrfptr $ \arrptr -> do
    #{poke TCOD_heightmap_t, w} p w
    #{poke TCOD_heightmap_t, h} p h
    #{poke TCOD_heightmap_t, values} p arrptr
    where
      arrfptr = toForeignPtr arr
      Z :. h :. w = extent arr
  peek p = do
    w <- #{peek TCOD_heightmap_t, w} p
    h <- #{peek TCOD_heightmap_t, h} p
    arrptr <- #{peek TCOD_heightmap_t, values} p
    fptr <- newForeignPtr_ arrptr
    pure $ TCODHeightMap $ fromForeignPtr (Z :. h :. w) fptr
