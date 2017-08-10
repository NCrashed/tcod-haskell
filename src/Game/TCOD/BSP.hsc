{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
-- | This toolkit allows one to create and manipulate 2D Binary Space Partition
-- trees. They can be used to split a rectangular region into non overlapping
-- sub-regions.
module Game.TCOD.BSP(
    BSP(..)
  , BSPCallback
  , bspNew
  , bspNewWithSize
  , bspDelete
  , bspLeft
  , bspRight
  , bspFather
  , bspIsLeaf
  , bspTraversePreOrder
  , bspTraverseInOrder
  , bspTraversePostOrder
  , bspTraverseLevelOrder
  , bspTraverseInvertedLevelOrder
  , bspContains
  , bspFindNode
  , bspResize
  , bspSplitOnce
  , bspSplitRecursive
  , bspRemoveSons
  ) where

import Data.Monoid
import Foreign
import Foreign.C
import Game.TCOD.Context as C
import Game.TCOD.MersenneTypes
import Game.TCOD.Tree
import GHC.Generics

context (tcodContext <> funCtx)
verbatim "#define TCOD_SDL2"
include "bsp.h"
#include "bsp.h"

-- | Data for BSP of map
data BSP = BSP {
  bspTree                :: !(TCODTree BSP) -- ^ BSP inherit tree
, bspX, bspY, bspW, bspH :: {-# UNPACK #-} !Int -- ^ Node position & size
, bspPosition            :: {-# UNPACK #-} !Int -- ^ Position of splitting
, bspLevel               :: {-# UNPACK #-} !Word8 -- ^ level int the tree
, bspHorizontal          :: !Bool -- ^ horizontal splitting?
} deriving (Eq, Show, Generic)

instance Storable BSP where
  sizeOf _ = #{size TCOD_bsp_t}
  alignment _ = #{alignment TCOD_bsp_t}
  poke p BSP{..} = do
    #{poke TCOD_bsp_t, tree} p bspTree
    #{poke TCOD_bsp_t, x} p bspX
    #{poke TCOD_bsp_t, y} p bspY
    #{poke TCOD_bsp_t, w} p bspW
    #{poke TCOD_bsp_t, h} p bspH
    #{poke TCOD_bsp_t, position} p bspPosition
    #{poke TCOD_bsp_t, level} p bspLevel
    #{poke TCOD_bsp_t, horizontal} p bspHorizontal
  peek p = BSP
    <$> (#{peek TCOD_bsp_t, tree} p)
    <*> (#{peek TCOD_bsp_t, x} p)
    <*> (#{peek TCOD_bsp_t, y} p)
    <*> (#{peek TCOD_bsp_t, w} p)
    <*> (#{peek TCOD_bsp_t, h} p)
    <*> (#{peek TCOD_bsp_t, position} p)
    <*> (#{peek TCOD_bsp_t, level} p)
    <*> (#{peek TCOD_bsp_t, horizontal} p)

-- | Callback for BSP traversal
type BSPCallback = BSP -> IO Bool

-- | Creating the root node
--
-- First, you have to create the root node of the tree. This node encompasses
-- the whole rectangular region.
bspNew :: IO BSP
bspNew = peek . castPtr =<< [C.exp| void* {TCOD_bsp_new()}|]

-- | Creating the root node
--
-- First, you have to create the root node of the tree. This node encompasses
-- the whole rectangular region.
bspNewWithSize :: Int -- ^ x Top left corner position and size of the rectangular region covered by the BSP tree.
  -> Int -- ^ y
  -> Int -- ^ w
  -> Int -- ^ h
  -> IO BSP
bspNewWithSize x y w h = do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
  peek . castPtr =<< [C.exp| void* {TCOD_bsp_new_with_size($(int x'), $(int y'), $(int w'), $(int h'))}|]

-- | Destroy BSP and all subtrees
bspDelete :: BSP -> IO ()
bspDelete bsp = do
  bsp' <- castPtr <$> new bsp
  [C.exp| void {TCOD_bsp_delete((TCOD_bsp_t*)$(void* bsp'))}|]

-- | Get left subtree
bspLeft :: BSP -> IO (Maybe BSP)
bspLeft bsp = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
  ptr <- [C.exp| void* { (void*)TCOD_bsp_left((TCOD_bsp_t*)$(void* bsp''))} |]
  if ptr == nullPtr then pure Nothing else Just <$> peek (castPtr ptr)

-- | Get right subtree
bspRight :: BSP -> IO (Maybe BSP)
bspRight bsp = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
  ptr <- [C.exp| void* { (void*)TCOD_bsp_right((TCOD_bsp_t*)$(void* bsp''))} |]
  if ptr == nullPtr then pure Nothing else Just <$> peek (castPtr ptr)

-- | Get parent subtree
bspFather :: BSP -> IO (Maybe BSP)
bspFather bsp = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
  ptr <- [C.exp| void* { (void*)TCOD_bsp_father((TCOD_bsp_t*)$(void* bsp''))} |]
  if ptr == nullPtr then pure Nothing else Just <$> peek (castPtr ptr)

-- | You can know if a node is a leaf (not split, no sons) with this function
bspIsLeaf :: BSP -> IO Bool
bspIsLeaf bsp = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
  toBool <$> [C.exp| int { (int)TCOD_bsp_is_leaf((TCOD_bsp_t*)$(void* bsp'')) } |]

-- | Traversing the tree
--
-- You can scan all the nodes of the tree and have a custom function called back
-- for each node. Each traversal function returns false if the traversal has been
-- interrupted (a callback returned false).
--
-- Pre-order : the callback is called for the current node, then for the left
-- son, then for the right son.
bspTraversePreOrder :: BSP -- ^ node
  -> BSPCallback
  -> IO Bool
bspTraversePreOrder bsp f = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      f' ptr _ = fromBool <$> (f =<< peek (castPtr ptr))
  toBool <$> [C.exp| int { (int)TCOD_bsp_traverse_pre_order((TCOD_bsp_t*)$(void* bsp''), (TCOD_bsp_callback_t)$fun:(int (*f')(void*, void*)), NULL) } |]

-- | Traversing the tree
--
-- You can scan all the nodes of the tree and have a custom function called back
-- for each node. Each traversal function returns false if the traversal has been
-- interrupted (a callback returned false).
--
-- In-order : the callback is called for the left son, then for current node,
-- then for the right son.
bspTraverseInOrder :: BSP -- ^ node
  -> BSPCallback
  -> IO Bool
bspTraverseInOrder bsp f = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      f' ptr _ = fromBool <$> (f =<< peek (castPtr ptr))
  toBool <$> [C.exp| int { (int)TCOD_bsp_traverse_in_order((TCOD_bsp_t*)$(void* bsp''), (TCOD_bsp_callback_t)$fun:(int (*f')(void*, void*)), NULL) } |]

-- | Traversing the tree
--
-- You can scan all the nodes of the tree and have a custom function called back
-- for each node. Each traversal function returns false if the traversal has been
-- interrupted (a callback returned false).
--
-- Post-order : the callback is called for the left son, then for the right son,
-- then for the current node.
bspTraversePostOrder :: BSP -- ^ node
  -> BSPCallback
  -> IO Bool
bspTraversePostOrder bsp f = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      f' ptr _ = fromBool <$> (f =<< peek (castPtr ptr))
  toBool <$> [C.exp| int { (int)TCOD_bsp_traverse_post_order((TCOD_bsp_t*)$(void* bsp''), (TCOD_bsp_callback_t)$fun:(int (*f')(void*, void*)), NULL) } |]

-- | Traversing the tree
--
-- You can scan all the nodes of the tree and have a custom function called back
-- for each node. Each traversal function returns false if the traversal has been
-- interrupted (a callback returned false).
--
-- Level-order : the callback is called for the nodes level by level, from left
-- to right.
bspTraverseLevelOrder :: BSP -- ^ node
  -> BSPCallback
  -> IO Bool
bspTraverseLevelOrder bsp f = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      f' ptr _ = fromBool <$> (f =<< peek (castPtr ptr))
  toBool <$> [C.exp| int { (int)TCOD_bsp_traverse_level_order((TCOD_bsp_t*)$(void* bsp''), (TCOD_bsp_callback_t)$fun:(int (*f')(void*, void*)), NULL) } |]

-- | Traversing the tree
--
-- You can scan all the nodes of the tree and have a custom function called back
-- for each node. Each traversal function returns false if the traversal has been
-- interrupted (a callback returned false).
--
-- Inverted level-order : the callback is called in the exact inverse order as
-- Level-order.
bspTraverseInvertedLevelOrder :: BSP -- ^ node
  -> BSPCallback
  -> IO Bool
bspTraverseInvertedLevelOrder bsp f = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      f' ptr _ = fromBool <$> (f =<< peek (castPtr ptr))
  toBool <$> [C.exp| int { (int)TCOD_bsp_traverse_inverted_level_order((TCOD_bsp_t*)$(void* bsp''), (TCOD_bsp_callback_t)$fun:(int (*f')(void*, void*)), NULL) } |]

-- | Check if a cell is inside a node
bspContains :: BSP
  -> Int -- ^ x
  -> Int -- ^ y
  -> IO Bool
bspContains bsp x y = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      x' = fromIntegral x
      y' = fromIntegral y
  toBool <$> [C.exp| int { (int)TCOD_bsp_contains((TCOD_bsp_t*)$(void* bsp''), $(int x'), $(int y')) } |]

-- | Getting the node containing a cell
bspFindNode :: BSP
  -> Int -- ^ x
  -> Int -- ^ y
  -> IO (Maybe BSP)
bspFindNode bsp x y = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      x' = fromIntegral x
      y' = fromIntegral y
  ptr <- castPtr <$> [C.exp| void* { (void*)TCOD_bsp_find_node((TCOD_bsp_t*)$(void* bsp''), $(int x'), $(int y')) } |]
  if ptr == nullPtr then pure Nothing else Just <$> peek (castPtr ptr)

-- | This operation resets the size of the tree nodes without changing the
-- splitting data (orientation/position). It should be called with the initial
-- region size or a bigger size, else some splitting position may be out of
-- the region.
--
-- You can use it if you changed the nodes size and position while using the
-- BSP tree, which happens typically when you use the tree to build a dungeon.
-- You create rooms inside the tree leafs, then shrink the leaf to fit the room
-- size. Calling resize on the root node with the original region size allows you
-- to reset all nodes to their original size.
bspResize :: BSP
  -> Int -- ^ x
  -> Int -- ^ y
  -> Int -- ^ w
  -> Int -- ^ h
  -> IO ()
bspResize bsp x y w h = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
  [C.exp| void { TCOD_bsp_resize((TCOD_bsp_t*)$(void* bsp''), $(int x'), $(int y'), $(int w'), $(int h')) } |]

-- | Splitting a node once
--
-- Once you have the root node, you can split it into two smaller non-overlapping nodes.
bspSplitOnce :: BSP
  -> Bool -- ^ horizontal?
  -> Int -- ^ position
  -> IO ()
bspSplitOnce bsp h p = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      h' = fromBool h
      p' = fromIntegral p
  [C.exp| void { TCOD_bsp_split_once((TCOD_bsp_t*)$(void* bsp''), $(int h')!=0, $(int p')) } |]

-- | Recursively splitting a node
--
-- You can also recursively split the bsp. At each step, a random orientation
-- (horizontal/vertical) and position are chosen
bspSplitRecursive :: BSP
  -> TCODRandom -- ^ randomize algorithm
  -> Int -- ^ Number of recursion levels.
  -> Int -- ^ min h size. Minimum values of w and h for a node. A node is split only if the resulting sub-nodes are bigger than minHSize x minVSize
  -> Int -- ^ min v size
  -> Float -- ^ max h ratio. Maximum values of w/h and h/w for a node. If a node does not conform, the splitting orientation is forced to reduce either the w/h or the h/w ratio. Use values near 1.0 to promote square nodes.
  -> Float -- ^ max v ratio
  -> IO ()
bspSplitRecursive bsp (TCODRandom r) nb mh mv mhr mvr = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
      nb' = fromIntegral nb
      mh' = fromIntegral mh
      mv' = fromIntegral mv
      mhr' = realToFrac mhr
      mvr' = realToFrac mvr
  [C.exp| void { TCOD_bsp_split_recursive((TCOD_bsp_t*)$(void* bsp''), $(void* r), $(int nb'), $(int mh'), $(int mv'), $(float mhr'), $(float mvr')) } |]

-- | Deleting a part of the tree
--
-- You can delete a part of the tree, releasing resources for all sub nodes with
bspRemoveSons :: BSP -> IO ()
bspRemoveSons bsp = with bsp $ \bsp' -> do
  let bsp'' = castPtr bsp'
  [C.exp| void { TCOD_bsp_remove_sons((TCOD_bsp_t*)$(void* bsp'')) } |]
