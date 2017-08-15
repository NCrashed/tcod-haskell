{-# LANGUAGE QuasiQuotes #-}
-- | This toolkit is an implementation of two fast and high quality pseudorandom number generators:
-- * a Mersenne twister generator,
-- * a Complementary-Multiply-With-Carry generator.
-- CMWC is faster than MT (see table below) and has a much better period
-- (1039460 vs. 106001). It is the default algo since libtcod 1.5.0.
module Game.TCOD.Mersenne(
    randomGetInstance
  , randomNew
  , randomSave
  , randomRestore
  , randomNewFromSeed
  , randomDelete
  , randomSetDistribution
  , randomGetInt
  , randomGetDouble
  , randomGetIntMean
  , randomGetDoubleMean
  , randomDiceNew
  , randomDiceRoll
  , randomDiceRollS
  ) where

import Foreign
import Foreign.C
import Game.TCOD.Context as C
import Game.TCOD.MersenneTypes
import Game.TCOD.Noise

context tcodContext
verbatim "#define TCOD_SDL2"
include "libtcod/mersenne.h"

-- | Default generator
--
-- The simplest way to get random number is to use the default generator.
-- The first time you get this generator, it is initialized by calling
-- TCOD_random_new. Then, on successive calls, this function returns the same
-- generator (singleton pattern).
randomGetInstance :: IO TCODRandom
randomGetInstance = TCODRandom <$> [C.exp| void* {TCOD_random_get_instance()} |]

-- | Generators with random seeds
--
-- You can also create as many generators as you want with a random seed
-- (the number of seconds since Jan 1 1970 at the time the constructor is called).
-- Warning ! If you call this function several times in the same second, it will
-- return the same generator.
randomNew :: RandomAlgorithm -> IO TCODRandom
randomNew algo = do
  let algo' = fromIntegral . fromEnum $ algo
  TCODRandom <$> [C.exp| void* { TCOD_random_new((TCOD_random_algo_t)$(int algo')) } |]

-- | Saving a RNG state into generator-backup
randomSave :: TCODRandom -> IO TCODRandom
randomSave (TCODRandom r) = TCODRandom <$> [C.exp| void* { TCOD_random_save($(void* r)) } |]

-- | Restoring a saved state
--
-- And restore it later. This makes it possible to get the same series of number
-- several times with a single generator.
randomRestore :: TCODRandom -- ^ generator that will be restored from backup
  -> TCODRandom -- ^ backup-generator, returned by 'randomSave'
  -> IO ()
randomRestore (TCODRandom r) (TCODRandom bp) = [C.exp| void { TCOD_random_restore($(void* r), $(void* bp)) } |]

-- | Generators with user defined seeds
--
-- Finally, you can create generators with a specific seed. Those allow you to
-- get a reproducible set of random numbers. You can for example save a dungeon
-- in a file by saving only the seed used for its generation (provided you have
-- a determinist generation algorithm)
randomNewFromSeed :: RandomAlgorithm -> Word -> IO TCODRandom
randomNewFromSeed algo s = do
  let algo' = fromIntegral . fromEnum $ algo
      s' = fromIntegral s
  TCODRandom <$> [C.exp| void* { TCOD_random_new_from_seed((TCOD_random_algo_t)$(int algo'), $(uint32_t s')) } |]

-- | Destroying a RNG
--
-- To release resources used by a generator, use the function.
-- NB : do not delete the default random generator !
randomDelete :: TCODRandom -> IO ()
randomDelete (TCODRandom r) = [C.exp|void {TCOD_random_delete($(void* r))}|]

-- | Setting the default RNG distribution
--
-- Note: See docs for 'Distribution'
randomSetDistribution :: TCODRandom -> Distribution -> IO ()
randomSetDistribution (TCODRandom r) d = do
  let d' = fromIntegral . fromEnum $ d
  [C.exp| void { TCOD_random_set_distribution($(void* r), (TCOD_distribution_t)$(int d')) } |]

-- | Getting an integer
--
-- Once you obtained a generator (using one of those methods), you can get random
-- numbers using the following functions, using either the explicit or simplified
-- API where applicable:
randomGetInt :: TCODRandom
  -> Int -- ^ min. Range of values returned. Each time you call this function, you get a number between (including) min and max
  -> Int -- ^ max
  -> IO Int
randomGetInt (TCODRandom r) minv maxv = do
  let minv' = fromIntegral minv
      maxv' = fromIntegral maxv
  fromIntegral <$> [C.exp| int { TCOD_random_get_int($(void* r), $(int minv'), $(int maxv')) } |]

-- | Getting a floating point number
--
-- Once you obtained a generator (using one of those methods), you can get random
-- numbers using the following functions, using either the explicit or simplified
-- API where applicable:
randomGetDouble :: TCODRandom
  -> Double -- ^ min. Range of values returned. Each time you call this function, you get a number between (including) min and max
  -> Double -- ^ max
  -> IO Double
randomGetDouble (TCODRandom r) minv maxv = do
  let minv' = realToFrac minv
      maxv' = realToFrac maxv
  fromIntegral <$> [C.exp| int { TCOD_random_get_double($(void* r), $(float minv'), $(float maxv')) } |]

-- | Getting an integer
--
-- Once you obtained a generator (using one of those methods), you can get random
-- numbers using the following functions, using either the explicit or simplified
-- API where applicable:
randomGetIntMean :: TCODRandom
  -> Int -- ^ min. Range of values returned. Each time you call this function, you get a number between (including) min and max
  -> Int -- ^ max
  -> Int -- ^ mean. This is used to set a custom mean, ie, not min+((max-min)/2). It can even be outside of the min-max range. Using a mean will force the use of a weighted (Gaussian) distribution, even if linear is set.
  -> IO Int
randomGetIntMean (TCODRandom r) minv maxv m = do
  let minv' = fromIntegral minv
      maxv' = fromIntegral maxv
      m' = fromIntegral m
  fromIntegral <$> [C.exp| int { TCOD_random_get_int_mean($(void* r), $(int minv'), $(int maxv'), $(int m')) } |]

-- | Getting a floating point number
--
-- Once you obtained a generator (using one of those methods), you can get random
-- numbers using the following functions, using either the explicit or simplified
-- API where applicable:
randomGetDoubleMean :: TCODRandom
  -> Double -- ^ min. Range of values returned. Each time you call this function, you get a number between (including) min and max
  -> Double -- ^ max
  -> Double -- ^ mean. This is used to set a custom mean, ie, not min+((max-min)/2). It can even be outside of the min-max range. Using a mean will force the use of a weighted (Gaussian) distribution, even if linear is set.
  -> IO Double
randomGetDoubleMean (TCODRandom r) minv maxv m = do
  let minv' = realToFrac minv
      maxv' = realToFrac maxv
      m' = realToFrac m
  realToFrac <$> [C.exp| float { TCOD_random_get_double_mean($(void* r), $(float minv'), $(float maxv'), $(float m')) } |]

-- | Create dnd-like dice from description string like "3d6+2"
randomDiceNew :: String -> IO Dice
randomDiceNew str = withCString str $ \str' -> alloca $ \res -> do
  [C.exp| void { *$(TCOD_dice_t* res) = TCOD_random_dice_new($(const char* str')) } |]
  peek res

-- | Roll dice and return resulted dice side
randomDiceRoll :: TCODRandom -> Dice -> IO Int
randomDiceRoll (TCODRandom r) d = with d $ \d' ->
  fromIntegral <$> [C.exp| int {TCOD_random_dice_roll($(void* r), *$(TCOD_dice_t* d'))}|]

-- | Roll dice without initiating, see 'randomDiceNew'
randomDiceRollS :: TCODRandom -> String -> IO Int
randomDiceRollS (TCODRandom r) str = withCString str $ \str' ->
  fromIntegral <$> [C.exp| int {TCOD_random_dice_roll_s($(void* r), $(const char* str'))}|]
