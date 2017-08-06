-- | Where we define C-side types to handle in inline-c
{-# LANGUAGE OverloadedLists #-}
module Game.TCOD.Context(
    tcodContext
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

tcodContext :: C.Context
tcodContext = mempty {
    C.ctxTypesTable = [
      -- (C.TypeName "?", [t| ? |])
    ]
  }
