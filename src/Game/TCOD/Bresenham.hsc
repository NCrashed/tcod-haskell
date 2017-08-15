{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
-- | This toolkit is a very simple and lightweight implementation of the
-- bresenham line drawing algorithm. It allows you to follow straight paths on
-- your map very easily.
module Game.TCOD.Bresenham(
    TCODLineListener
  , lineInitGlobal
  , lineStepGlobal
  , lineGlobal
  , TCODBresenhamData(..)
  , lineInit
  , lineStep
  , lineIO
  , line
  ) where

import Data.Monoid
import Foreign
import Foreign.C
import Game.TCOD.Context as C
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

context (tcodContext <> funCtx)
verbatim "#define TCOD_SDL2"
include "libtcod/bresenham.h"
#include "libtcod/bresenham.h"

-- | Takes x y of point and tells when to stop
type TCODLineListener = Int -> Int -> IO Bool

-- | Initializing the line
--
-- First, you have to initialize the toolkit with your starting and ending coordinates.
lineInitGlobal :: Int -- ^ x from
  -> Int -- ^ y from
  -> Int -- ^ x to
  -> Int -- ^ y to
  -> IO ()
lineInitGlobal xf yf xt yt = do
  let xf' = fromIntegral xf
      yf' = fromIntegral yf
      xt' = fromIntegral xt
      yt' = fromIntegral yt
  [C.exp| void { TCOD_line_init($(int xf'), $(int yf'), $(int xt'), $(int yt')) } |]

-- | Walking the line
--
-- You can then step through each cell with this function. It returns true when you reach the line's ending point.
lineStepGlobal :: IO (Bool, Int, Int)
lineStepGlobal = alloca $ \xp -> alloca $ \yp -> do
  isEnd <- toBool <$> [C.exp| int {(int)TCOD_line_step($(int* xp), $(int* yp))}|]
  let pk = fmap fromIntegral . peek
  (,,) <$> pure isEnd <*> pk xp <*> pk yp

-- | Callback-based function. Stops when the callback returns false
--
-- The function returns false if the line has been interrupted by the callback
-- (it returned false before the last point).
lineGlobal :: Int -- ^ x from
  -> Int -- ^ y from
  -> Int -- ^ x to
  -> Int -- ^ y to
  -> TCODLineListener -- ^ Callback called for each line's point. The function stops if the callback returns false.
  -> IO Bool
lineGlobal xf yf xt yt cb = do
  let xf' = fromIntegral xf
      yf' = fromIntegral yf
      xt' = fromIntegral xt
      yt' = fromIntegral yt
      cb' x y = fromBool <$> cb (fromIntegral x) (fromIntegral y)
  toBool <$> [C.exp| int { (int)TCOD_line($(int xf'), $(int yf'), $(int xt'), $(int yt'), (TCOD_line_listener_t)$fun:(int (*cb')(int, int))) } |]

-- | Holds state of Bresenham algorithm for thread safe version of API
data TCODBresenhamData = TCODBresenhamData {
  bresenStepX  :: {-# UNPACK #-} !Int
, bresenStepY  :: {-# UNPACK #-} !Int
, bresenE      :: {-# UNPACK #-} !Int
, bresenDeltaX :: {-# UNPACK #-} !Int
, bresenDeltaY :: {-# UNPACK #-} !Int
, bresenOrigX  :: {-# UNPACK #-} !Int
, bresenOrigY  :: {-# UNPACK #-} !Int
, bresenDestX  :: {-# UNPACK #-} !Int
, bresenDestY  :: {-# UNPACK #-} !Int
} deriving (Eq, Show, Generic)

instance Storable TCODBresenhamData where
  sizeOf _ = #{size TCOD_bresenham_data_t}
  alignment _ = #{alignment TCOD_bresenham_data_t}
  poke p TCODBresenhamData{..} = do
    #{poke TCOD_bresenham_data_t, stepx} p bresenStepX
    #{poke TCOD_bresenham_data_t, stepy} p bresenStepY
    #{poke TCOD_bresenham_data_t, e} p bresenE
    #{poke TCOD_bresenham_data_t, deltax} p bresenDeltaX
    #{poke TCOD_bresenham_data_t, deltay} p bresenDeltaY
    #{poke TCOD_bresenham_data_t, origx} p bresenOrigX
    #{poke TCOD_bresenham_data_t, origy} p bresenOrigY
    #{poke TCOD_bresenham_data_t, destx} p bresenDestX
    #{poke TCOD_bresenham_data_t, desty} p bresenDestY
  peek p = TCODBresenhamData
    <$> (#{peek TCOD_bresenham_data_t, stepx} p)
    <*> (#{peek TCOD_bresenham_data_t, stepy} p)
    <*> (#{peek TCOD_bresenham_data_t, e} p)
    <*> (#{peek TCOD_bresenham_data_t, deltax} p)
    <*> (#{peek TCOD_bresenham_data_t, deltay} p)
    <*> (#{peek TCOD_bresenham_data_t, origx} p)
    <*> (#{peek TCOD_bresenham_data_t, origy} p)
    <*> (#{peek TCOD_bresenham_data_t, destx} p)
    <*> (#{peek TCOD_bresenham_data_t, desty} p)

-- | Initializing the line, thread safe version
--
-- First, you have to initialize the toolkit with your starting and ending coordinates.
lineInit ::Int -- ^ x from
  -> Int -- ^ y from
  -> Int -- ^ x to
  -> Int -- ^ y to
  -> TCODBresenhamData -- ^ State of algorithm that is passed around
lineInit xf yf xt yt = unsafePerformIO $ alloca $ \dp -> do
  let xf' = fromIntegral xf
      yf' = fromIntegral yf
      xt' = fromIntegral xt
      yt' = fromIntegral yt
      dp' = castPtr dp
  [C.exp| void { TCOD_line_init_mt($(int xf'), $(int yf'), $(int xt'), $(int yt'), (TCOD_bresenham_data_t*)$(void* dp')) } |]
  peek dp

-- | Walking the line, thread safe version
--
-- You can then step through each cell with this function. It returns true when you reach the line's ending point.
lineStep :: TCODBresenhamData -> (TCODBresenhamData, Bool, Int, Int)
lineStep d = unsafePerformIO $ with d $ \dp -> alloca $ \xp -> alloca $ \yp -> do
  let dp' = castPtr dp
  isEnd <- toBool <$> [C.exp| int {(int)TCOD_line_step_mt($(int* xp), $(int* yp), (TCOD_bresenham_data_t*)$(void* dp'))}|]
  let pk = fmap fromIntegral . peek
  (,,,) <$> peek dp <*> pure isEnd <*> pk xp <*> pk yp

-- | Callback-based function. Stops when the callback returns false, thread safe version
--
-- The function returns false if the line has been interrupted by the callback
-- (it returned false before the last point).
lineIO :: Int -- ^ x from
  -> Int -- ^ y from
  -> Int -- ^ x to
  -> Int -- ^ y to
  -> TCODLineListener -- ^ Callback called for each line's point. The function stops if the callback returns false.
  -> IO Bool
lineIO xf yf xt yt cb = alloca $ \(dp :: Ptr TCODBresenhamData) -> do
  let dp' = castPtr dp
      xf' = fromIntegral xf
      yf' = fromIntegral yf
      xt' = fromIntegral xt
      yt' = fromIntegral yt
      cb' x y = fromBool <$> cb (fromIntegral x) (fromIntegral y)
  toBool <$> [C.exp| int { (int)TCOD_line_mt($(int xf'), $(int yf'), $(int xt'), $(int yt'), (TCOD_line_listener_t)$fun:(int (*cb')(int, int)), (TCOD_bresenham_data_t*)$(void* dp')) } |]

-- | Callback-based function. Stops when the callback returns false, thread safe version
--
-- The function returns false if the line has been interrupted by the callback
-- (it returned false before the last point).
--
-- Note: that predicated is pure unlike in 'lineIO'
line :: Int -- ^ x from
  -> Int -- ^ y from
  -> Int -- ^ x to
  -> Int -- ^ y to
  -> (Int -> Int -> Bool) -- ^ Callback called for each line's point. The function stops if the callback returns false.
  -> Bool
line xf yf xt yt cb = unsafePerformIO $ lineIO xf yf xt yt $ \x y -> pure $ cb x y
