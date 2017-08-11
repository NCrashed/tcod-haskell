{-# LANGUAGE QuasiQuotes #-}
module Game.TCOD.Noise(
    TCODNoise(..)
  , TCODNoiseType(..)
  , noiseMaxOctaves
  , noiseMaxDimensions
  , noiseDefaultHurst
  , noiseDefaultLacunarity
  , noiseNew
  , noiseDelete
  , noiseSetType
  , noiseGetEx
  , noiseGetFbmEx
  , noiseGetTurbulenceEx
  , noiseGet
  , noiseGetFbm
  , noiseGetTurbulence
  ) where

import Data.Vector.Storable (Vector)
import Foreign
import Foreign.C
import Game.TCOD.Context as C
import Game.TCOD.MersenneTypes
import GHC.Generics

import qualified Data.Vector.Storable as V

context tcodContext
verbatim "#define TCOD_SDL2"
include "noise.h"

-- | Reference to TCOD noise object
newtype TCODNoise = TCODNoise { unTCODNoise :: Ptr () }
  deriving (Eq, Ord, Show, Generic)

-- | Supported noise generator types
data TCODNoiseType =
    NoiseDefault
  | NoisePerlin
  | NoiseSimplex
  | NoiseWavelet
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

noiseMaxOctaves :: Int
noiseMaxOctaves = 128

noiseMaxDimensions :: Int
noiseMaxDimensions = 4

noiseDefaultHurst :: Double
noiseDefaultHurst = 0.5

noiseDefaultLacunarity :: Double
noiseDefaultLacunarity = 2.0

-- | Create a new noise object
noiseNew :: Int -- ^ Dimensions
  -> Double -- ^ hurst
  -> Double -- ^ lacunarity
  -> TCODRandom -- ^ random number generator
  -> IO TCODNoise
noiseNew d h l (TCODRandom r) = do
  let d' = fromIntegral d
      h' = realToFrac h
      l' = realToFrac l
  TCODNoise <$> [C.exp| void* { TCOD_noise_new($(int d'), $(float h'), $(float l'), $(void* r)) } |]

-- | Delete noise object
noiseDelete :: TCODNoise -> IO ()
noiseDelete (TCODNoise n) = [C.exp| void { TCOD_noise_delete($(void* n)) }|]

-- | Choosing a noise type
--
-- Use this function to define the default algorithm used by the noise functions.
-- The default algorithm is simplex. It's much faster than Perlin, especially in 4 dimensions.
-- It has a better contrast too.
noiseSetType :: TCODNoise -> TCODNoiseType -> IO ()
noiseSetType (TCODNoise n) nt = do
  let nt' = fromIntegral . fromEnum $ nt
  [C.exp| void { TCOD_noise_set_type($(void* n), (TCOD_noise_type_t)$(int nt')) } |]

-- | Getting flat noise
--
-- his function returns the noise function value between -1.0 and 1.0 at given coordinates.
noiseGetEx :: TCODNoise -- ^ the generator handler, returned by the initialization function.
  -> Vector Double -- ^ An array of coordinates, depending on the generator dimensions (between 1 and 4). The same array of coordinates will always return the same value.
  -> TCODNoiseType -- ^ The algorithm to use. If not defined, use the default one (set with setType or simplex if not set)
  -> IO Double
noiseGetEx (TCODNoise n) vf nt = V.unsafeWith (V.map realToFrac vf) $ \vf' -> do
  let nt' = fromIntegral . fromEnum $ nt
  realToFrac <$> [C.exp| float { TCOD_noise_get_ex($(void* n), $(float* vf'), (TCOD_noise_type_t)$(int nt')) } |]

-- | This function returns the fbm function value between -1.0 and 1.0 at given
-- coordinates, using fractal hurst and lacunarity defined when the generator has been created.
noiseGetFbmEx :: TCODNoise -- ^ the generator handler, returned by the initialization function.
  -> Vector Double -- ^ An array of coordinates, depending on the generator dimensions (between 1 and 4). The same array of coordinates will always return the same value.
  -> Float -- ^ Number of iterations. Must be < TCOD_NOISE_MAX_OCTAVES = 128
  -> TCODNoiseType -- ^ The algorithm to use
  -> IO Float
noiseGetFbmEx (TCODNoise n) vf o nt = V.unsafeWith (V.map realToFrac vf) $ \vf' -> do
  let nt' = fromIntegral . fromEnum $ nt
      o' = realToFrac o
  realToFrac <$> [C.exp| float { TCOD_noise_get_fbm_ex($(void* n), $(float* vf'), $(float o'), (TCOD_noise_type_t)$(int nt')) } |]

-- | Getting turbulence
--
-- This function returns the turbulence function value between -1.0 and 1.0 at
-- given coordinates, using fractal hurst and lacunarity defined when the
-- generator has been created.
noiseGetTurbulenceEx :: TCODNoise -- ^ the generator handler, returned by the initialization function.
  -> Vector Double -- ^ An array of coordinates, depending on the generator dimensions (between 1 and 4). The same array of coordinates will always return the same value.
  -> Float -- ^ Number of iterations. Must be < TCOD_NOISE_MAX_OCTAVES = 128
  -> TCODNoiseType -- ^ The algorithm to use
  -> IO Float
noiseGetTurbulenceEx (TCODNoise n) vf o nt = V.unsafeWith (V.map realToFrac vf) $ \vf' -> do
  let nt' = fromIntegral . fromEnum $ nt
      o' = realToFrac o
  realToFrac <$> [C.exp| float { TCOD_noise_get_turbulence_ex($(void* n), $(float* vf'), $(float o'), (TCOD_noise_type_t)$(int nt')) } |]

-- | Getting flat noise
--
-- his function returns the noise function value between -1.0 and 1.0 at given coordinates.
noiseGet :: TCODNoise -- ^ the generator handler, returned by the initialization function.
  -> Vector Double -- ^ An array of coordinates, depending on the generator dimensions (between 1 and 4). The same array of coordinates will always return the same value.
  -> IO Double
noiseGet (TCODNoise n) vf = V.unsafeWith (V.map realToFrac vf) $ \vf' ->
  realToFrac <$> [C.exp| float { TCOD_noise_get($(void* n), $(float* vf')) } |]

-- | This function returns the fbm function value between -1.0 and 1.0 at given
-- coordinates, using fractal hurst and lacunarity defined when the generator has been created.
noiseGetFbm :: TCODNoise -- ^ the generator handler, returned by the initialization function.
  -> Vector Double -- ^ An array of coordinates, depending on the generator dimensions (between 1 and 4). The same array of coordinates will always return the same value.
  -> Float -- ^ Number of iterations. Must be < TCOD_NOISE_MAX_OCTAVES = 128
  -> IO Float
noiseGetFbm (TCODNoise n) vf o = V.unsafeWith (V.map realToFrac vf) $ \vf' -> do
  let o' = realToFrac o
  realToFrac <$> [C.exp| float { TCOD_noise_get_fbm($(void* n), $(float* vf'), $(float o')) } |]

-- | Getting turbulence
--
-- This function returns the turbulence function value between -1.0 and 1.0 at
-- given coordinates, using fractal hurst and lacunarity defined when the
-- generator has been created.
noiseGetTurbulence :: TCODNoise -- ^ the generator handler, returned by the initialization function.
  -> Vector Double -- ^ An array of coordinates, depending on the generator dimensions (between 1 and 4). The same array of coordinates will always return the same value.
  -> Float -- ^ Number of iterations. Must be < TCOD_NOISE_MAX_OCTAVES = 128
  -> IO Float
noiseGetTurbulence (TCODNoise n) vf o = V.unsafeWith (V.map realToFrac vf) $ \vf' -> do
  let o' = realToFrac o
  realToFrac <$> [C.exp| float { TCOD_noise_get_turbulence($(void* n), $(float* vf'), $(float o')) } |]
