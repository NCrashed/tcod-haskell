{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
module Game.TCOD.BSP(

  ) where

import Data.Monoid
import Foreign
import Foreign.C
import Game.TCOD.Context as C
import GHC.Generics

context (tcodContext <> funCtx)
verbatim "#define TCOD_SDL2"
include "bsp.h"
#include "bsp.h"
