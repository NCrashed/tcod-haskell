module Game.TCOD.Version(
    tcodHexVersion
  , tcodVersion
  , tcodVersionName
  , tcodTechVersion
  ) where

import Game.TCOD.Context as C
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C

context tcodContext
include "libtcod/version.h"

tcodHexVersion :: Word
tcodHexVersion = unsafePerformIO $ fromIntegral <$> [C.exp| int {TCOD_HEXVERSION} |]

tcodVersion :: String
tcodVersion = unsafePerformIO $ peekCString =<< [C.exp| const char* {TCOD_STRVERSION} |]

tcodVersionName :: String
tcodVersionName = unsafePerformIO $ peekCString =<< [C.exp| const char* {TCOD_STRVERSIONNAME} |]

tcodTechVersion :: Word
tcodTechVersion = unsafePerformIO $ fromIntegral <$> [C.exp| int {TCOD_TECHVERSION} |]
