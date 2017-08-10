{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
module Game.TCOD.MersenneTypes(
    Dice(..)
  , RandomAlgorithm(..)
  , Distribution(..)
  ) where

import Foreign
import GHC.Generics

#include "mersenne_types.h"

-- | Dice roll
data Dice = Dice {
  diceNbRolls     :: {-# UNPACK #-} !Int
, diceNbFaces     :: {-# UNPACK #-} !Int
, diceMultiplier  :: {-# UNPACK #-} !Double
, diceAddSub      :: {-# UNPACK #-} !Double
} deriving (Eq, Show, Generic)

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
data RandomAlgorithm = RngMT | RngCMWC
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Random number distribution laws
data Distribution =
    DistributionLinear
  | DistributionGaussian
  | DistributionGaussianRange
  | DistributionGaussianInverse
  | DistributionGaussianRangeInverse
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
