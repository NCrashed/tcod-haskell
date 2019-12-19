{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
module Game.TCOD.Tree(
    TCODTree(..)
  , treeNew
  , treeAddSon
  ) where

import Foreign
import Foreign.C
import GHC.Generics

import Game.TCOD.Context as C

context tcodContext
verbatim "#define TCOD_SDL2"
include "libtcod/portability.h"
include "libtcod/tree.h"
#include "libtcod/tree.h"

-- | Rose tree implemented in TCOD
data TCODTree a = TCODTree {
  treeNext   :: {-# UNPACK #-} !(Ptr (TCODTree a))
, treeFather :: {-# UNPACK #-} !(Ptr (TCODTree a))
, treeSons   :: {-# UNPACK #-} !(Ptr (TCODTree a))
} deriving (Eq, Show, Generic)

instance Storable (TCODTree a) where
  sizeOf _ = #{size TCOD_tree_t}
  alignment _ = #{alignment TCOD_tree_t}
  poke p TCODTree{..} =  do
    #{poke TCOD_tree_t, next} p treeNext
    #{poke TCOD_tree_t, father} p treeFather
    #{poke TCOD_tree_t, sons} p treeSons
  peek p = TCODTree
    <$> (#{peek TCOD_tree_t, next} p)
    <*> (#{peek TCOD_tree_t, father} p)
    <*> (#{peek TCOD_tree_t, sons} p)

-- | Allocating new tree node
treeNew :: IO (TCODTree a)
treeNew = peek . castPtr =<< [C.exp| void* { (void*)TCOD_tree_new() } |]

-- | Adding a new son to tree
treeAddSon :: TCODTree a -- ^ node
  -> TCODTree a -- ^ son
  -> IO ()
treeAddSon n s = with n $ \n' -> with s $ \s' -> do
  let n'' = castPtr n'
      s'' = castPtr s'
  [C.exp| void {
    TCOD_tree_add_son((TCOD_tree_t*)$(void* n''), (TCOD_tree_t*)$(void* s''))
  }|]
