{-# LANGUAGE QuasiQuotes #-}
-- | This toolkit allows one to easily calculate the optimal path between two points in your dungeon by using either the <a href="http://en.wikipedia.org/wiki/A*">A* algorithm</a> or <a href="http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm">Dijkstra's algorithm</a>.
-- Please note that the paths generated with the two algorithms may differ slightly. Due to how they're implemented, A* will usually prefer diagonal moves over orthogonal, while Dijkstra will have the opposite preference. In other words, paths from point X to point Y will look like this:
-- @@
-- Dijkstra:      A*:
-- ..........   ..........
-- .X........   .X*.......
-- ..*.......   ...**.....
-- ...*......   .....**...
-- ....****Y.   .......*Y.
-- ..........   ..........
-- @@
module Game.TCOD.Path(
    TCODPathFunc
  , TCODPathFuncRaw
  , TCODPath(..)
  , pathNewUsingMap
  , pathNewUsingFunction
  , pathDelete
  , pathCompute
  , pathWalk
  , pathIsEmpty
  , pathSize
  , pathReverse
  , pathGet
  , pathGetOrigin
  , pathGetDesitnation
  ) where

import Data.Monoid
import Foreign
import Foreign.C
import Game.TCOD.Context as C
import Game.TCOD.Fov
import GHC.Generics

import Language.C.Inline (mkFunPtr)

context (tcodContext <> funCtx)
verbatim "#define TCOD_SDL2"
include "path.h"

-- | Callback path function, takes from (x, y) and to (x, y), returns cell cost
--
-- A custom function that must return the walk cost from coordinates xFrom,yFrom to coordinates xTo,yTo.
-- The cost must be > 0.0f if the cell xTo,yTo is walkable.
-- It must be equal to 0.0f if it's not.
-- You must not take additional cost due to diagonal movements into account as it's already done by the pathfinder.
type TCODPathFunc = Int -> Int -> Int -> Int -> IO Double

-- | C type of 'TCODPathFunc'
type TCODPathFuncRaw = CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CFloat

-- | Reference to TCOD path finder state
data TCODPath = TCODPath {
    unTCODPath  :: Ptr () -- ^ Original pointer
  -- | Stored reference to callback that need to be freed after cleanup of the path object
  , pathHaskFun :: Maybe (FunPtr TCODPathFuncRaw)
  }
  deriving (Eq, Show, Ord, Generic)

-- | Allocating a pathfinder from a map
--
-- First, you have to allocate a path using a map from 'Game.TCOD.Fov' module.
pathNewUsingMap :: TCODMap
  -> Float -- ^ diagonal cost. Cost of a diagonal movement compared to an horizontal or vertical movement. On a standard cartesian map, it should be sqrt(2) (1.41f).
           -- It you want the same cost for all movements, use 1.0f.
           -- If you don't want the path finder to use diagonal movements, use 0.0f.
  -> IO TCODPath
pathNewUsingMap (TCODMap m) dc = do
  let dc' = realToFrac dc
  flip TCODPath Nothing <$> [C.exp| void * { TCOD_path_new_using_map($(void* m), $(float dc')) } |]

-- | Allocating a pathfinder using a callback
--
-- Since the walkable status of a cell may depend on a lot of parameters
-- (the creature type, the weather, the terrain type...), you can also create a
-- path by providing a function rather than relying on a TCODMap.
pathNewUsingFunction :: Int -- ^ map width
  -> Int -- ^ map height
  -> TCODPathFunc -- ^ A custom function that must return the walk cost from coordinates xFrom,yFrom to coordinates xTo,yTo.
                  -- The cost must be > 0.0f if the cell xTo,yTo is walkable.
                  -- It must be equal to 0.0f if it's not.
                  -- You must not take additional cost due to diagonal movements into account as it's already done by the pathfinder.
  -> Float -- ^ diagonal cost. Cost of a diagonal movement compared to an horizontal or vertical movement. On a standard cartesian map, it should be sqrt(2) (1.41f).
           -- It you want the same cost for all movements, use 1.0f.
           -- If you don't want the path finder to use diagonal movements, use 0.0f.
  -> IO TCODPath
pathNewUsingFunction mw mh f dc = do
  let mw' = fromIntegral mw
      mh' = fromIntegral mh
      dc' = realToFrac dc
      f' xF yF xT yT _ = realToFrac <$> f (fromIntegral xF) (fromIntegral yF) (fromIntegral xT) (fromIntegral yT)
  callback <- $(mkFunPtr [t| CInt -> CInt -> CInt -> CInt -> Ptr () -> IO CFloat |]) f'
  let callback' = castFunPtrToPtr callback
  ptr <- [C.exp| void * { TCOD_path_new_using_function($(int mw'), $(int mh'), (TCOD_path_func_t)$(void* callback'), NULL, $(float dc'))} |]
  pure $ TCODPath ptr (Just callback)

-- | To release the resources used by a path, destroy it
pathDelete :: TCODPath -> IO ()
pathDelete (TCODPath p mc) = do
  [C.exp| void {TCOD_path_delete($(void* p))} |]
  maybe (pure ()) freeHaskellFunPtr mc

-- | Computing an A* path
--
-- Once you created a TCODPath object, you can compute the path between two points.
-- Both points should be inside the map, and at a walkable position. The function
-- returns false if there is no possible path.
pathCompute :: TCODPath
  -> Int -- ^ ox. Coordinates of the origin of the path.
  -> Int -- ^ oy. Coordinates of the origin of the path.
  -> Int -- ^ dx. Coordinates of the destination of the path.
  -> Int -- ^ dy. Coordinates of the destination of the path.
  -> IO Bool
pathCompute (TCODPath p _) ox oy dx dy = do
  let ox' = fromIntegral ox
      oy' = fromIntegral oy
      dx' = fromIntegral dx
      dy' = fromIntegral dy
  toBool <$> [C.exp| int { (int)TCOD_path_compute($(void* p), $(int ox'), $(int oy'), $(int dx'), $(int dy'))}|]

-- | Walking the path
--
-- You can walk the path and go to the next step with :
-- Note that walking the path consume one step (and decrease the path size by one).
-- The function returns false if recalculateWhenNeeded is false and the next cell
-- on the path is no longer walkable, or if recalculateWhenNeeded is true, the
-- next cell on the path is no longer walkable and no other path has been found.
-- Also note that recalculateWhenNeeded only applies to A*.
pathWalk :: TCODPath
  -> Bool -- ^ recalculate when needed. If the next point is no longer walkable (another creature may be in the way), recalculate a new path and walk it.
  -> IO (Bool, Int, Int)
pathWalk (TCODPath p _) r = alloca $ \x -> alloca $ \y-> do
  let r' = fromBool r
  fin <- toBool <$> [C.exp| int {(int)TCOD_path_walk($(void* p), $(int* x), $(int* y), $(int r')!=0)}|]
  let pk = fmap fromIntegral . peek
  (,,) <$> pure fin <*> pk x <*> pk y

-- | Check is path has no steps
pathIsEmpty :: TCODPath -> IO Bool
pathIsEmpty (TCODPath p _) = toBool <$> [C.exp| int {(int)TCOD_path_is_empty($(void* p))}|]

-- | Get number of steps the path consists of
pathSize :: TCODPath -> IO Int
pathSize (TCODPath p _) = fromIntegral <$> [C.exp| int { TCOD_path_size($(void * p))}|]

-- | Reversing a path
--
-- Once you computed a path, you can exchange origin and destination
pathReverse :: TCODPath -> IO ()
pathReverse (TCODPath p _) = [C.exp| void {TCOD_path_reverse($(void* p))} |]

-- | Read the path cells' coordinates
--
-- You can get the coordinates of each point along the path
pathGet :: TCODPath
  -> Int -- ^ index. Step number. 0 <= index < path size
  -> IO (Int, Int)
pathGet (TCODPath p _) i = alloca $ \x -> alloca $ \y -> do
  let i' = fromIntegral i
  [C.exp| void {TCOD_path_get($(void* p), $(int i'), $(int* x), $(int* y))}|]
  let pk = fmap fromIntegral . peek
  (,) <$> pk x <*> pk y

-- | Get origin/start of the path
pathGetOrigin :: TCODPath -> IO (Int, Int)
pathGetOrigin (TCODPath p _) = alloca $ \x -> alloca $ \y -> do
  [C.exp| void {TCOD_path_get_origin($(void* p), $(int* x), $(int* y))}|]
  let pk = fmap fromIntegral . peek
  (,) <$> pk x <*> pk y

-- | Get destination/end of the path
pathGetDesitnation :: TCODPath -> IO (Int, Int)
pathGetDesitnation (TCODPath p _) = alloca $ \x -> alloca $ \y -> do
  [C.exp| void {TCOD_path_get_destination($(void* p), $(int* x), $(int* y))}|]
  let pk = fmap fromIntegral . peek
  (,) <$> pk x <*> pk y
