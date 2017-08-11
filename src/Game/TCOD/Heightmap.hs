{-# LANGUAGE QuasiQuotes #-}
module Game.TCOD.Heightmap(
    heightmapNew
  , heightmapDelete
  , heightmapGetValue
  , heightmapGetInterpolatedValue
  , heightmapSetValue
  , heightmapGetSlope
  , heightmapGetNormal
  , heightmapCountCells
  , heightmapHasLandOnBorder
  , heightmapGetMinMax
  , heightmapCopy
  , heightmapAdd
  , heightmapScale
  , heightmapClamp
  , heightmapNormalize
  , heightmapClear
  , heightmapLerp
  , heightmapAddHm
  , heightmapMultiplyHm
  , heightmapAddHill
  , heightmapDigHill
  , DigBezier(..)
  , heightmapDigBezier
  , heightmapRainErosion
  , heightmapKernelTransform
  , heightmapAddVoronoi
  , heightmapMidPointDisplacement
  , heightmapAddFbm
  , heightmapScaleFbm
  , heightmapIslandify
  ) where

import Data.Vector.Storable (Vector)
import Foreign
import Foreign.C
import Game.TCOD.Context as C
import Game.TCOD.HeightmapInst
import Game.TCOD.MersenneTypes
import Game.TCOD.Noise

import qualified Data.Vector.Storable as V

context tcodContext
verbatim "#define TCOD_SDL2"
include "heightmap.h"

-- | Allocate new heightmap with given sizes
heightmapNew :: Int -> Int -> IO TCODHeightMap
heightmapNew w h = do
  let w' = fromIntegral w
      h' = fromIntegral h
  peek =<< [C.exp| TCOD_heightmap_t* { TCOD_heightmap_new($(int w'), $(int h')) } |]

-- | Destroy inner buffers of heightmap
heightmapDelete :: TCODHeightMap -> IO ()
heightmapDelete m = with m $ \m' -> [C.exp| void { TCOD_heightmap_delete($(TCOD_heightmap_t* m')) } |]

-- | Get value of heightmap at given point
heightmapGetValue :: TCODHeightMap -> Int -> Int -> IO Double
heightmapGetValue m x y = with m $ \m' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
  realToFrac <$> [C.exp| float { TCOD_heightmap_get_value($(TCOD_heightmap_t* m'), $(int x'), $(int y')) } |]

-- | This function returns the interpolated height at non integer coordinates.
heightmapGetInterpolatedValue :: TCODHeightMap -> Double -> Double -> IO Double
heightmapGetInterpolatedValue m x y = with m $ \m' -> do
  let x' = realToFrac x
      y' = realToFrac y
  realToFrac <$> [C.exp| float { TCOD_heightmap_get_interpolated_value($(TCOD_heightmap_t* m'), $(float x'), $(float y')) } |]

-- | Once the heightmap has been created, you can do some basic operations on the values inside it.
heightmapSetValue :: TCODHeightMap -> Int -> Int -> Double -> IO ()
heightmapSetValue m x y v = with m $ \m' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      v' = realToFrac v
  [C.exp| void { TCOD_heightmap_set_value($(TCOD_heightmap_t* m'), $(int x'), $(int y'), $(float v')) } |]

-- | This function returns the slope between 0 and PI/2 at given coordinates.
heightmapGetSlope :: TCODHeightMap -> Int -> Int -> IO Double
heightmapGetSlope m x y = with m $ \m' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
  realToFrac <$> [C.exp| float { TCOD_heightmap_get_slope($(TCOD_heightmap_t* m'), $(int x'), $(int y')) } |]

-- | This function returns the map normal at given coordinates.
heightmapGetNormal :: TCODHeightMap
  -> Double -- ^ x
  -> Double -- ^ y
  -> Double -- ^ Water level (default 0). The map height is clamped at waterLevel so that the sea is flat.
  -> IO (Double, Double, Double)
heightmapGetNormal m x y wl = with m $ \m' -> alloca $ \nx -> alloca $ \ny -> alloca $ \nz -> do
  let x' = realToFrac x
      y' = realToFrac y
      wl' = realToFrac wl
  [C.block| void {
    float n[3];
    TCOD_heightmap_get_normal($(TCOD_heightmap_t* m'), $(float x'), $(float y'), n, $(float wl'));
    *$(float* nx) = n[0];
    *$(float* ny) = n[1];
    *$(float* nz) = n[2];
  }|]
  let pk = fmap realToFrac . peek
  (,,) <$> pk nx <*> pk ny <*> pk nz

-- | Count the map cells inside a height range
--
-- This function returns the number of map cells which value is between min and max.
heightmapCountCells :: TCODHeightMap
  -> Double -- ^ min
  -> Double -- ^ max
  -> IO Int
heightmapCountCells m minv maxv = with m $ \m' -> do
  let minv' = realToFrac minv
      maxv' = realToFrac maxv
  fromIntegral <$> [C.exp| int { TCOD_heightmap_count_cells($(TCOD_heightmap_t* m'), $(float minv'), $(float maxv'))} |]

-- | Check if the map is an island
--
-- This function checks if the cells on the map border are below a certain height.
heightmapHasLandOnBorder :: TCODHeightMap
  -> Double -- ^ Return true only if no border cell is > waterLevel.
  -> IO Bool
heightmapHasLandOnBorder m wl = with m $ \m' -> do
  let wl' = realToFrac wl
  toBool <$> [C.exp| int { TCOD_heightmap_has_land_on_border($(TCOD_heightmap_t* m'), $(float wl')) } |]

-- | Get the map min and max values
heightmapGetMinMax :: TCODHeightMap
  -> IO (Double, Double)
heightmapGetMinMax m = with m $ \m' -> alloca $ \minv' -> alloca $ \maxv' -> do
  [C.exp| void { TCOD_heightmap_get_minmax($(TCOD_heightmap_t* m'), $(float* minv'), $(float* maxv')) } |]
  let pk = fmap realToFrac . peek
  (,) <$> pk minv' <*> pk maxv'

-- | Copy contents of heightmap from one to another
heightmapCopy :: TCODHeightMap -> TCODHeightMap -> IO ()
heightmapCopy m1 m2 = with m1 $ \m1' -> with m2 $ \m2' ->
  [C.exp| void {TCOD_heightmap_copy($(TCOD_heightmap_t* m1'), $(TCOD_heightmap_t* m2'))} |]

-- | Adding a float value to all cells
heightmapAdd :: TCODHeightMap -> Double -> IO ()
heightmapAdd m v = with m $ \m' -> do
  let v' = realToFrac v
  [C.exp| void {TCOD_heightmap_add($(TCOD_heightmap_t* m'), $(float v'))}|]

-- | Clamping all values
heightmapScale :: TCODHeightMap -> Double -> IO ()
heightmapScale m v = with m $ \m' -> do
  let v' = realToFrac v
  [C.exp| void {TCOD_heightmap_scale($(TCOD_heightmap_t* m'), $(float v'))}|]

-- | Clamping all values
heightmapClamp :: TCODHeightMap -> Double -> Double -> IO ()
heightmapClamp m minv maxv = with m $ \m' -> do
  let minv' = realToFrac minv
      maxv' = realToFrac maxv
  [C.exp| void {TCOD_heightmap_clamp($(TCOD_heightmap_t* m'), $(float minv'), $(float maxv'))}|]

-- | The whole heightmap is translated and scaled so that the lowest cell value
-- becomes min and the highest cell value becomes max
heightmapNormalize :: TCODHeightMap -> Double -> Double -> IO ()
heightmapNormalize m minv maxv = with m $ \m' -> do
  let minv' = realToFrac minv
      maxv' = realToFrac maxv
  [C.exp| void {TCOD_heightmap_normalize($(TCOD_heightmap_t* m'), $(float minv'), $(float maxv'))}|]

-- | Resetting all values to 0.0
heightmapClear :: TCODHeightMap -> IO ()
heightmapClear m = with m $ \m' ->
  [C.exp| void {TCOD_heightmap_clear($(TCOD_heightmap_t* m'))}|]

-- | Doing a lerp operation between two heightmaps
heightmapLerp :: TCODHeightMap -- ^ First heightmap in the lerp operation.
  -> TCODHeightMap -- ^ Second heightmap in the lerp operation.
  -> TCODHeightMap -- ^ Where to store result
  -> Double -- ^ coef	lerp coefficient. For each cell in the destination map, value = a.value + (b.value - a.value) * coef
  -> IO ()
heightmapLerp m1 m2 mr c = with m1 $ \m1' -> with m2 $ \m2' -> with mr $ \mr' -> do
  let c' = realToFrac c
  [C.exp| void { TCOD_heightmap_lerp_hm($(TCOD_heightmap_t* m1'), $(TCOD_heightmap_t* m2'), $(TCOD_heightmap_t* mr'), $(float c')) } |]

-- | Adding two heightmaps
heightmapAddHm :: TCODHeightMap -- ^ First heightmap in the addition operation.
  -> TCODHeightMap -- ^ Second heightmap in the addition operation.
  -> TCODHeightMap -- ^ Where to store result
  -> IO ()
heightmapAddHm m1 m2 mr = with m1 $ \m1' -> with m2 $ \m2' -> with mr $ \mr' ->
  [C.exp| void { TCOD_heightmap_add_hm($(TCOD_heightmap_t* m1'), $(TCOD_heightmap_t* m2'), $(TCOD_heightmap_t* mr')) } |]

-- | Multiplying values of two heightmaps
heightmapMultiplyHm :: TCODHeightMap -- ^ First heightmap in the addition operation.
  -> TCODHeightMap -- ^ Second heightmap in the addition operation.
  -> TCODHeightMap -- ^ Where to store result
  -> IO ()
heightmapMultiplyHm m1 m2 mr = with m1 $ \m1' -> with m2 $ \m2' -> with mr $ \mr' ->
  [C.exp| void { TCOD_heightmap_multiply_hm($(TCOD_heightmap_t* m1'), $(TCOD_heightmap_t* m2'), $(TCOD_heightmap_t* mr')) } |]

-- | Add hills
--
-- This function adds a hill (a half spheroid) at given position.
heightmapAddHill :: TCODHeightMap
  -> Double -- ^ hx
  -> Double -- ^ hy
  -> Double -- ^ radius
  -> Double -- ^ height
  -> IO ()
heightmapAddHill m hx hy r h = with m $ \m' -> do
  let hx' = realToFrac hx
      hy' = realToFrac hy
      r' = realToFrac r
      h' = realToFrac h
  [C.exp| void { TCOD_heightmap_add_hill($(TCOD_heightmap_t* m'), $(float hx'), $(float hy'), $(float r'), $(float h')) } |]

-- | Digg hills
--
-- This function digs a hill (a half spheroid) at given position.
heightmapDigHill :: TCODHeightMap
  -> Double -- ^ hx
  -> Double -- ^ hy
  -> Double -- ^ radius
  -> Double -- ^ height
  -> IO ()
heightmapDigHill m hx hy r h = with m $ \m' -> do
  let hx' = realToFrac hx
      hy' = realToFrac hy
      r' = realToFrac r
      h' = realToFrac h
  [C.exp| void { TCOD_heightmap_dig_hill($(TCOD_heightmap_t* m'), $(float hx'), $(float hy'), $(float r'), $(float h')) } |]

-- | Helper struct for 'heightmapDigBezier'
data DigBezier = DigBezier {
  bezierP1 :: (Int, Int)
, bezierP2 :: (Int, Int)
, bezierP3 :: (Int, Int)
, bezierP4 :: (Int, Int)
, bezierStartRadius :: !Double
, bezierStartDepth  :: !Double
, bezierEndRadius   :: !Double
, bezierEndDepth    :: !Double
}

-- | Digg hills
--
-- This function digs a hill (a half spheroid) at given position.
heightmapDigBezier :: TCODHeightMap
  -> DigBezier
  -> IO ()
heightmapDigBezier m DigBezier{..} = with m $ \m' -> do
  let p1x = fromIntegral . fst $ bezierP1
      p2x = fromIntegral . fst $ bezierP2
      p3x = fromIntegral . fst $ bezierP3
      p4x = fromIntegral . fst $ bezierP4
      p1y = fromIntegral . fst $ bezierP1
      p2y = fromIntegral . snd $ bezierP2
      p3y = fromIntegral . snd $ bezierP3
      p4y = fromIntegral . snd $ bezierP4
      sr = realToFrac bezierStartRadius
      sd = realToFrac bezierStartDepth
      er = realToFrac bezierEndRadius
      ed = realToFrac bezierEndDepth
  [C.block| void {
    int px[4] = { $(int p1x), $(int p2x), $(int p3x), $(int p4x) };
    int py[4] = { $(int p1y), $(int p2y), $(int p3y), $(int p4y) };
    TCOD_heightmap_dig_bezier($(TCOD_heightmap_t* m'), px, py, $(float sr), $(float sd), $(float er), $(float ed));
  } |]

-- | Simulate rain erosion
--
-- This function simulates the effect of rain drops on the terrain, resulting in erosion patterns.
heightmapRainErosion :: TCODHeightMap
  -> Int -- ^ number of drops. Number of rain drops to simulate. Should be at least width * height.
  -> Double -- ^ erosion coeff. Amount of ground eroded on the drop's path.
  -> Double -- ^ sedimantation coeff. Amount of ground deposited when the drops stops to flow
  -> TCODRandom -- ^ RNG to use, NULL for default generator.
  -> IO ()
heightmapRainErosion m n e s (TCODRandom r) = with m $ \m' -> do
  let n' = fromIntegral n
      e' = realToFrac e
      s' = realToFrac s
  [C.exp| void { TCOD_heightmap_rain_erosion($(TCOD_heightmap_t* m'), $(int n'), $(float e'), $(float s'), $(void* r)) } |]

-- | Do a generic transformation
--
-- This function allows you to apply a generic transformation on the map, so
-- that each resulting cell value is the weighted sum of several neighbour cells.
-- This can be used to smooth/sharpen the map. See examples below for a simple
-- horizontal smoothing kernel : replace value(x,y) with 0.33*value(x-1,y)
-- + 0.33*value(x,y) + 0.33*value(x+1,y).
--
-- To do this, you need a kernel of size 3 (the sum involves 3 surrounding cells).
heightmapKernelTransform :: TCODHeightMap
  -> Int -- ^ Kernel size. Number of neighbour cells involved. dx, dy and weights should be the exact same size of the value.
  -> Vector Int -- ^ dx. Array of kernelSize cells coordinates. The coordinates are relative to the current cell (0,0) is current cell, (-1,0) is west cell, (0,-1) is north cell, (1,0) is east cell, (0,1) is south cell, ...
  -> Vector Int -- ^ dy
  -> Vector Double -- ^ weights. Array of kernelSize cells weight. The value of each neighbour cell is scaled by its corresponding weight
  -> Double -- ^ min level. The transformation is only applied to cells which value is >= minLevel.
  -> Double -- ^ max level. The transformation is only applied to cells which value is <= maxLevel.
  -> IO ()
heightmapKernelTransform m ks dx dy ws minl maxl = with m $ \m' ->
  V.unsafeWith (V.map fromIntegral dx) $ \dx' ->
  V.unsafeWith (V.map fromIntegral dy) $ \dy' ->
  V.unsafeWith (V.map realToFrac ws) $ \ws' -> do
  let ks' = fromIntegral ks
      minl' = realToFrac minl
      maxl' = realToFrac maxl
  [C.exp| void { TCOD_heightmap_kernel_transform($(TCOD_heightmap_t* m'), $(int ks'), $(int* dx'), $(int* dy'), $(float* ws'), $(float minl'), $(float maxl')) } |]

-- | Add a Voronoi diagram
--
-- This function adds values from a Voronoi diagram to the map.
heightmapAddVoronoi :: TCODHeightMap
  -> Int -- ^ Number of Voronoi sites.
  -> Vector Double -- ^ coeff The distance to each site is scaled by the corresponding coef.
                   -- Closest site : coef[0], second closest site : coef[1], ...
  -> TCODRandom -- ^ RNG to use, NULL for default generator.
  -> IO ()
heightmapAddVoronoi m nb coeffs (TCODRandom r) = with m $ \m' -> V.unsafeWith (V.map realToFrac coeffs) $ \coeffs' -> do
  let nb' = fromIntegral nb
      nc' = fromIntegral $ V.length coeffs
  [C.exp| void { TCOD_heightmap_add_voronoi($(TCOD_heightmap_t* m'), $(int nb'), $(int nc'), $(float* coeffs'), $(void* r)) } |]

-- | Generate a map with mid-point displacement
--
-- This algorithm generates a realistic fractal heightmap using the <a href="http://en.wikipedia.org/wiki/Diamond-square_algorithm">diamond-square</a> (or random midpoint displacement) algorithm.
-- The roughness range should be comprised between 0.4 and 0.6. The image below show the same map with roughness varying from 0.4 to 0.6.
-- <img src="midpoint.png" />
-- It's also a good habit to normalize the map after using this algorithm to avoid unexpected heights.
heightmapMidPointDisplacement :: TCODHeightMap
  -> TCODRandom -- ^ Random number generation to use, or NULL/0 to use the default one.
  -> Double -- ^ roughness
  -> IO ()
heightmapMidPointDisplacement m (TCODRandom r) rs = with m $ \m' -> do
  let rs' = realToFrac rs
  [C.exp| void { TCOD_heightmap_mid_point_displacement($(TCOD_heightmap_t* m'), $(void* r), $(float rs')) } |]

-- | This function adds values from a simplex fbm function to the map.
heightmapAddFbm :: TCODHeightMap
  -> TCODNoise -- ^ The 2D noise to use.
  -> Double -- ^ mult x. mulx, muly / addx, addy	The noise coordinate for map cell (x,y) are (x + addx)*mulx / width , (y + addy)*muly / height.
            -- Those values allow you to scale and translate the noise function over the heightmap.
  -> Double -- ^ mult y
  -> Double -- ^ add x
  -> Double -- ^ add y
  -> Double -- ^ octaves. Number of octaves in the fbm sum.
  -> Double -- ^ delta. The value added to the heightmap is delta + noise * scale.
  -> Double -- ^ scale is between -1.0 and 1.0
  -> IO ()
heightmapAddFbm m (TCODNoise n) mx my ax ay o d s = with m $ \m' -> do
  let mx' = realToFrac mx
      my' = realToFrac my
      ax' = realToFrac ax
      ay' = realToFrac ay
      o' = realToFrac o
      d' = realToFrac d
      s' = realToFrac s
  [C.exp| void { TCOD_heightmap_add_fbm($(TCOD_heightmap_t* m'), $(void* n), $(float mx'), $(float my'), $(float ax'), $(float ay'), $(float o'), $(float d'), $(float s')) } |]

-- | This function adds values from a simplex fbm function to the map.
heightmapScaleFbm :: TCODHeightMap
  -> TCODNoise -- ^ The 2D noise to use.
  -> Double -- ^ mult x. mulx, muly / addx, addy	The noise coordinate for map cell (x,y) are (x + addx)*mulx / width , (y + addy)*muly / height.
            -- Those values allow you to scale and translate the noise function over the heightmap.
  -> Double -- ^ mult y
  -> Double -- ^ add x
  -> Double -- ^ add y
  -> Double -- ^ octaves. Number of octaves in the fbm sum.
  -> Double -- ^ delta. The value added to the heightmap is delta + noise * scale.
  -> Double -- ^ scale is between -1.0 and 1.0
  -> IO ()
heightmapScaleFbm m (TCODNoise n) mx my ax ay o d s = with m $ \m' -> do
  let mx' = realToFrac mx
      my' = realToFrac my
      ax' = realToFrac ax
      ay' = realToFrac ay
      o' = realToFrac o
      d' = realToFrac d
      s' = realToFrac s
  [C.exp| void { TCOD_heightmap_scale_fbm($(TCOD_heightmap_t* m'), $(void* n), $(float mx'), $(float my'), $(float ax'), $(float ay'), $(float o'), $(float d'), $(float s')) } |]

-- | Lowers the terrain near the heightmap borders
heightmapIslandify :: TCODHeightMap
  -> Double -- ^ sea level
  -> TCODRandom -- ^ Random number generation to use, or NULL/0 to use the default one.
  -> IO ()
heightmapIslandify m sl (TCODRandom r) = with m $ \m' -> do
  let sl' = realToFrac sl
  [C.exp| void { TCOD_heightmap_islandify($(TCOD_heightmap_t* m'), $(float sl'), $(void* r)) } |]
