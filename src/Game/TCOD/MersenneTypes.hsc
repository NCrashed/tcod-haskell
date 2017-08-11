{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
module Game.TCOD.MersenneTypes(
    TCODRandom(..)
  , Dice(..)
  , RandomAlgorithm(..)
  , Distribution(..)
  ) where

import Foreign
import GHC.Generics
import Game.TCOD.Context

#include "mersenne_types.h"

-- | Reference to TCOD pseudo random generator
newtype TCODRandom = TCODRandom { unTCODRandom :: Ptr () }
  deriving (Eq, Ord, Show, Generic)

instance Storable Dice where
  sizeOf _ = #{size TCOD_dice_t}
  alignment _ = #{alignment TCOD_dice_t}
  poke p Dice{..} = do
    #{poke TCOD_dice_t, nb_rolls} p diceNbRolls
    #{poke TCOD_dice_t, nb_faces} p diceNbFaces
    #{poke TCOD_dice_t, multiplier} p diceMultiplier
    #{poke TCOD_dice_t, addsub} p diceAddSub
  peek p = Dice
    <$> (#{peek TCOD_dice_t, nb_rolls} p)
    <*> (#{peek TCOD_dice_t, nb_faces} p)
    <*> (#{peek TCOD_dice_t, multiplier} p)
    <*> (#{peek TCOD_dice_t, addsub} p)

-- | Pseudo random number algorithm
data RandomAlgorithm = RngMT -- ^ a Mersenne twister generator
  | RngCMWC -- ^  a Complementary-Multiply-With-Carry generator.
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Random number distribution laws
data Distribution =
  -- | This is the default distribution. It will return a number from a range min-max.
  -- The numbers will be evenly distributed, ie, each number from the range has the
  -- exact same chance of being selected.
    DistributionLinear
  -- | This distribution does not have minimum and maximum values. Instead, a
  -- mean and a standard deviation are used. The mean is the central value. It
  -- will appear with the greatest frequency. The farther away from the mean,
  -- the less the probability of appearing the possible results have. Although
  -- extreme values are possible, 99.7% of the results will be within the radius
  -- of 3 standard deviations from the mean. So, if the mean is 0 and the standard
  -- deviation is 5, the numbers will mostly fall in the (-15,15) range.
  | DistributionGaussian
  -- | This one takes minimum and maximum values. Under the hood, it computes
  -- the mean (which falls right between the minimum and maximum) and the standard
  -- deviation and applies a standard Gaussian distribution to the values. The
  -- difference is that the result is always guaranteed to be in the min-max range.
  | DistributionGaussianRange
  -- | Essentially, this is the same as 'DistributionGaussian'. The difference
  -- is that the values near +3 and -3 standard deviations from the mean have
  -- the highest possibility of appearing, while the mean has the lowest.
  | DistributionGaussianInverse
  -- | Essentially, this is the same as 'DistributionGaussianRange', but
  -- the min and max values have the greatest probability of appearing, while
  -- the values between them, the lowest.
  | DistributionGaussianRangeInverse
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
