module Game.TCOD.Heightmap(

  ) where

import Game.TCOD.Context as C
import Game.TCOD.MersenneTypes
import Game.TCOD.Noise

context tcodContext
verbatim "#define TCOD_SDL2"
include "heightmap.h"
