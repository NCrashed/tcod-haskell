{-# LANGUAGE QuasiQuotes #-}
module Game.TCOD.Fov(
    TCODMap(..)
  , mapNew
  , mapClear
  , mapCopy
  , mapSetProperties
  , mapDelete
  , mapComputeFov
  , mapIsInFov
  , mapSetInFov
  , mapIsTransparent
  , mapIsWalkable
  , mapGetWidth
  , mapGetHeight
  , mapGetNumberCells
  ) where

import Foreign
import Foreign.C
import Game.TCOD.Context as C
import Game.TCOD.FovTypes
import GHC.Generics

context tcodContext
verbatim "#define TCOD_SDL2"
include "fov.h"

-- | Map reference for FOV algorithm
newtype TCODMap = TCODMap { unTCODMap :: Ptr () }
  deriving (Eq, Ord, Show, Generic)

-- | Creating the map object
--
-- First, you have to allocate a map of the same size as your dungeon.
mapNew :: Int -- ^ width The size of the map (in map cells).
  -> Int -- ^ height The size of the map (in map cells).
  -> IO TCODMap
mapNew w h = do
  let w' = fromIntegral w
      h' = fromIntegral h
  TCODMap <$> [C.exp| void* {TCOD_map_new($(int w'), $(int h'))} |]

-- | Clearing the map
--
-- You can clear an existing map (setting all cells to the chosen walkable/transparent values)
mapClear :: TCODMap
  -> Bool -- ^ transparent Whether the cells should be transparent.
  -> Bool -- ^ walkable Whether the cells should be walkable.
  -> IO ()
mapClear (TCODMap m) t w = do
  let t' = fromBool t
      w' = fromBool w
  [C.exp| void { TCOD_map_clear($(void* m), $(int t')!=0, $(int w')!=0) } |]

-- | Copy a map to another, reallocating it when needed
mapCopy :: TCODMap -- ^ source
  -> TCODMap -- ^ destination
  -> IO ()
mapCopy (TCODMap m1) (TCODMap m2) = [C.exp| void { TCOD_map_copy($(void* m1), $(void* m2)) } |]

-- | Defining the cell properties
--
-- Then, build your dungeon by defining which cells let the light pass (by default,
-- all cells block the light) and which cells are walkable (by default, all cells
-- are not-walkable).
mapSetProperties :: TCODMap -- ^ map
  -> Int -- ^ x Coordinate of the cell that we want to update.
  -> Int -- ^ y Coordinate of the cell that we want to update.
  -> Bool -- ^ is transparent If true, this cell will let the light pass else it will block the light.
  -> Bool -- ^ is walkable If true, creatures can walk true this cell (it is not a wall).
  -> IO ()
mapSetProperties (TCODMap m) x y t w = do
  let x' = fromIntegral x
      y' = fromIntegral y
      t' = fromBool t
      w' = fromBool w
  [C.exp| void {TCOD_map_set_properties($(void* m), $(int x'), $(int y'), $(int t')!=0, $(int w')!=0)} |]

-- | Destroy a map
mapDelete :: TCODMap -> IO ()
mapDelete (TCODMap m) = [C.exp| void { TCOD_map_delete($(void* m)) }|]

-- | Computing the field of view
--
-- Once your map is allocated and empty cells have been defined, you can calculate the field of view
mapComputeFov :: TCODMap -- ^ map
  -> Int -- ^ player x. Position of the player in the map.
  -> Int -- ^ player y. Position of the player in the map.
  -> Int -- ^ max radius. If > 0, the fov is only computed up to maxRadius cells away from the player. Else, the range is unlimited.
  -> Bool -- ^ light walls. Whether the wall cells near ground cells in fov must be in fov too.
  -> TCODFovAlgorithm -- ^ FOV algorithm to use.
  -> IO ()
mapComputeFov (TCODMap m) px py mr lw alg = do
  let px' = fromIntegral px
      py' = fromIntegral py
      mr' = fromIntegral mr
      lw' = fromBool lw
      alg' = fromIntegral . fromEnum $ alg
  [C.exp| void { TCOD_map_compute_fov($(void* m), $(int px'), $(int py'), $(int mr'), $(int lw')!=0, (TCOD_fov_algorithm_t)$(int alg')) } |]

-- | Check if a cell is in the last computed field of view
mapIsInFov :: TCODMap
  -> Int -- ^ x
  -> Int -- ^ y
  -> IO Bool
mapIsInFov (TCODMap m) x y = do
  let x' = fromIntegral x
      y' = fromIntegral y
  toBool <$> [C.exp| int { (int)TCOD_map_is_in_fov($(void* m), $(int x'), $(int y')) } |]

-- | Manually setting a cell in fov
mapSetInFov :: TCODMap
  -> Int -- ^ x
  -> Int -- ^ y
  -> Bool -- ^ fov
  -> IO ()
mapSetInFov (TCODMap m) x y f = do
  let x' = fromIntegral x
      y' = fromIntegral y
      f' = fromBool f
  [C.exp| void { TCOD_map_set_in_fov($(void* m), $(int x'), $(int y'), $(int f')!=0) } |]

-- | Getting transparency flag for cell
mapIsTransparent :: TCODMap
  -> Int -- ^ x
  -> Int -- ^ y
  -> IO Bool
mapIsTransparent (TCODMap m) x y = do
  let x' = fromIntegral x
      y' = fromIntegral y
  toBool <$> [C.exp| int { (int)TCOD_map_is_transparent($(void* m), $(int x'), $(int y')) } |]

-- | Getting walkability flag for cell
mapIsWalkable :: TCODMap
  -> Int -- ^ x
  -> Int -- ^ y
  -> IO Bool
mapIsWalkable (TCODMap m) x y = do
  let x' = fromIntegral x
      y' = fromIntegral y
  toBool <$> [C.exp| int { (int)TCOD_map_is_walkable($(void* m), $(int x'), $(int y')) } |]

-- | Getting width of the map
mapGetWidth :: TCODMap -> IO Int
mapGetWidth (TCODMap m) = fromIntegral <$>
  [C.exp| int {TCOD_map_get_width($(void* m))}|]

-- | Getting height of the map
mapGetHeight :: TCODMap -> IO Int
mapGetHeight (TCODMap m) = fromIntegral <$>
  [C.exp| int {TCOD_map_get_height($(void* m))}|]

-- | Getting number of cells of the map
mapGetNumberCells :: TCODMap -> IO Int
mapGetNumberCells (TCODMap m) = fromIntegral <$>
  [C.exp| int {TCOD_map_get_nb_cells($(void* m))}|]
