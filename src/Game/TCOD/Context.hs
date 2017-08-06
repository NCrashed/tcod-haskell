-- | Where we define C-side types to handle in inline-c
{-# LANGUAGE OverloadedLists #-}
module Game.TCOD.Context(
    tcodContext
  -- * Types
  , Color(..)
  -- * Reexports
  , C.context
  , C.include
  , C.verbatim
  , C.exp
  , C.pure
  , C.block
  ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Data.Word
import GHC.Generics

-- | 24 bit color
data Color = Color {
  colorR :: {-# UNPACK #-} !Word8
, colorG :: {-# UNPACK #-} !Word8
, colorB :: {-# UNPACK #-} !Word8
} deriving (Eq, Show, Generic)

tcodContext :: C.Context
tcodContext = mempty {
    C.ctxTypesTable = [
      (C.TypeName "TCOD_color_t", [t| Color |])
    ]
  }
