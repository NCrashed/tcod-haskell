module Game.TCOD.List(
    TCODList(..)
  , listNew
  , listAllocate
  , listDuplicated
  , listDelete
  , listPush
  , listPushPtr
  , listPop
  , listPopPtr
  , listPeek
  , listPeekPtr
  , listAddAll
  , listGet
  , listGetPtr
  , listSet
  , listSetPtr
  , listBeginPtr
  , listEndPtr
  , listReverse
  , listRemoveIterator
  , listRemoveIteratorFast
  , listRemovePtr
  , listRemove
  , listRemoveFastPtr
  , listRemoveFast
  , listContainsPtr
  , listContains
  , listClear
  , listClearAndDelete
  , listSize
  , listInsertBeforePtr
  , listInsertBefore
  , listIsEmpty
  -- * Haskell conversion
  , listToList
  , listFromList
  , listToVectorUnsafe
  , listToVector
  ) where

import Foreign
import Foreign.C
import GHC.Generics
import Data.Vector.Storable (Vector)

import qualified Data.Vector.Storable as V

import Game.TCOD.Context as C

context tcodContext
include "list.h"

-- | This is a fast, lightweight and generic container, that provides array,
-- list and stack paradigms.
newtype TCODList a = TCODList { unTCODList :: Ptr () }
  deriving (Eq, Ord, Show, Generic)

-- | Creating a list
listNew :: IO (TCODList a)
listNew = TCODList <$> [C.exp| void * { TCOD_list_new() } |]

-- | You can also create an empty list and pre-allocate memory for elements.
-- Use this if you know the list size and want the memory to fit it perfectly.
listAllocate :: Int -- ^ Number of elements
  -> IO (TCODList a)
listAllocate n = TCODList <$> [C.exp| void* {TCOD_list_allocate($(int n'))} |]
  where n' = fromIntegral n

-- | You can create a list by duplicating an existing list.
listDuplicated :: TCODList a -> IO (TCODList a)
listDuplicated (TCODList l) = TCODList <$> [C.exp| void* {TCOD_list_duplicate($(void* l))}|]

-- | You can delete a list, freeing any allocated resources. Note that deleting
-- the list does not delete it's elements. You have to use clearAndDelete before
-- deleting the list if you want to destroy the elements too.
listDelete :: TCODList a -> IO ()
listDelete (TCODList l) = [C.exp| void {TCOD_list_delete($(void* l))} |]

-- | You can push an element on the stack (append it to the end of the list)
listPush :: Storable a => TCODList a -> a -> IO ()
listPush l v = listPushPtr l =<< new v

-- | You can push an element on the stack (append it to the end of the list)
listPushPtr :: TCODList a -> Ptr a -> IO ()
listPushPtr (TCODList l) vptr = do
  let vptr' = castPtr vptr
  [C.exp| void { TCOD_list_push($(void* l), $(void* vptr'))} |]

-- | You can pop an element from the stack (remove the last element of the list).
listPop :: Storable a => TCODList a -> IO (Maybe a)
listPop l = do
  ptr <- listPopPtr l
  if ptr == nullPtr then pure Nothing else Just <$> peek ptr

-- | You can pop an element from the stack (remove the last element of the list).
listPopPtr :: TCODList a -> IO (Ptr a)
listPopPtr (TCODList l) = castPtr <$> [C.exp| void* { TCOD_list_pop($(void* l))} |]

-- | You can read the last element of the stack without removing it :
listPeek :: Storable a => TCODList a -> IO (Maybe a)
listPeek l = do
  ptr <- listPeekPtr l
  if ptr == nullPtr then pure Nothing else Just <$> peek ptr

-- | You can read the last element of the stack without removing it :
listPeekPtr :: TCODList a -> IO (Ptr a)
listPeekPtr (TCODList l) = castPtr <$> [C.exp| void* { TCOD_list_peek($(void* l))} |]

-- | You can concatenate two lists. Every element of l2 will be added to current
-- list
listAddAll :: TCODList a -> TCODList a -> IO ()
listAddAll (TCODList l1) (TCODList l2) = [C.exp| void { TCOD_list_add_all($(void* l1), $(void* l2))} |]

-- | You can retrieve a value with get.
listGet :: Storable a => TCODList a -> Int -> IO (Maybe a)
listGet l n = do
  ptr <- listGetPtr l n
  if ptr == nullPtr then pure Nothing else Just <$> peek ptr

-- | You can retrieve a value with get.
listGetPtr :: TCODList a -> Int -> IO (Ptr a)
listGetPtr (TCODList l) n = do
  let n' = fromIntegral n
  castPtr <$> [C.exp| void* {TCOD_list_get($(void* l), $(int n'))} |]

-- | You can assign a value with set. If needed, the array will allocate new elements up to idx.
listSet :: Storable a => TCODList a -> a -> Int -> IO ()
listSet l v n = do
  vptr <- new v
  listSetPtr l vptr n

-- | You can assign a value with set. If needed, the array will allocate new elements up to idx.
listSetPtr :: TCODList a -> Ptr a -> Int -> IO ()
listSetPtr (TCODList l) vptr n = do
  let vptr' = castPtr vptr
      n' = fromIntegral n
  [C.exp| void { TCOD_list_set($(void* l), $(void* vptr'), $(int n')) } |]

-- | You can iterate through the elements of the list using an iterator.
-- begin() returns the address of the first element of the list. You go to the
-- next element using the increment operator ++. When the iterator's value is
-- equal to end(), you've gone through all the elements.
--
-- Warning ! You cannot insert elements in the list while iterating through it.
-- Inserting elements can result in reallocation of the list and your iterator
-- will not longer be valid.
listBeginPtr :: TCODList a -> IO (Ptr (Ptr a))
listBeginPtr (TCODList l) = castPtr <$> [C.exp| void** { TCOD_list_begin($(void* l)) } |]

-- | You can iterate through the elements of the list using an iterator.
-- begin() returns the address of the first element of the list. You go to the
-- next element using the increment operator ++. When the iterator's value is
-- equal to end(), you've gone through all the elements.
--
-- Warning ! You cannot insert elements in the list while iterating through it.
-- Inserting elements can result in reallocation of the list and your iterator
-- will not longer be valid.
listEndPtr :: TCODList a -> IO (Ptr (Ptr a))
listEndPtr (TCODList l) = castPtr <$> [C.exp| void** { TCOD_list_end($(void* l)) } |]

-- | This function reverses the order of the elements in the list.
listReverse :: TCODList a -> IO ()
listReverse (TCODList l) = [C.exp| void { TCOD_list_reverse($(void* l)) } |]

-- | You can remove an element from the list while iterating. The element at
-- the iterator position will be removed. The function returns the new iterator.
-- The _fast versions replace the element to remove with the last element of the
-- list. They're faster, but do not preserve the list order.
listRemoveIterator :: TCODList a -> Ptr (Ptr a) -> IO (Ptr (Ptr a))
listRemoveIterator (TCODList l) ptr = do
  let ptr' = castPtr ptr
  castPtr <$> [C.exp| void** {TCOD_list_remove_iterator($(void* l), $(void** ptr'))} |]

-- | You can remove an element from the list while iterating. The element at
-- the iterator position will be removed. The function returns the new iterator.
-- The _fast versions replace the element to remove with the last element of the
-- list. They're faster, but do not preserve the list order.
listRemoveIteratorFast :: TCODList a -> Ptr (Ptr a) -> IO (Ptr (Ptr a))
listRemoveIteratorFast (TCODList l) ptr = do
  let ptr' = castPtr ptr
  castPtr <$> [C.exp| void** {TCOD_list_remove_iterator_fast($(void* l), $(void** ptr'))} |]

-- | Removing an element from the list
listRemovePtr :: TCODList a -> Ptr a -> IO ()
listRemovePtr (TCODList l) vptr = do
  let vptr' = castPtr vptr
  [C.exp| void { TCOD_list_remove($(void* l), $(void* vptr')) } |]

-- | Removing an element from the list
listRemove :: Storable a => TCODList a -> a -> IO ()
listRemove l v = do
  vptr <- new v
  listRemovePtr l vptr

-- | The _fast versions replace the element to remove with the last element of
-- the list. They're faster, but do not preserve the list order.
listRemoveFastPtr :: TCODList a -> Ptr a -> IO ()
listRemoveFastPtr (TCODList l) vptr = do
  let vptr' = castPtr vptr
  [C.exp| void { TCOD_list_remove_fast($(void* l), $(void* vptr')) } |]

-- | The _fast versions replace the element to remove with the last element of
-- the list. They're faster, but do not preserve the list order.
listRemoveFast :: Storable a => TCODList a -> a -> IO ()
listRemoveFast l v = do
  vptr <- new v
  listRemoveFastPtr l vptr

-- | Checking if an element is in the list
listContainsPtr :: TCODList a -> Ptr a -> IO Bool
listContainsPtr (TCODList l) vptr = do
  let vptr' = castPtr vptr
  toBool <$> [C.exp| int {(int)TCOD_list_contains($(void* l), $(void* vptr'))}|]

-- | Checking if an element is in the list
listContains :: Storable a => TCODList a -> a -> IO Bool
listContains l a = with a $ listContainsPtr l

-- | Emptying a list
listClear :: TCODList a -> IO ()
listClear (TCODList l) = [C.exp| void {TCOD_list_clear($(void* l))} |]

-- | For lists containing pointers, you can clear the list and delete
-- (or free for C) the elements :
listClearAndDelete :: TCODList a -> IO ()
listClearAndDelete (TCODList l) = [C.exp| void {TCOD_list_clear_and_delete($(void* l))} |]

-- | Getting the list size
listSize :: TCODList a -> IO Int
listSize (TCODList l) = fromIntegral <$> [C.exp| int {TCOD_list_size($(void* l))} |]

-- | Insert an element in the list
listInsertBeforePtr :: TCODList a -> Ptr a -> Int -> IO (Ptr (Ptr a))
listInsertBeforePtr (TCODList l) vptr n = do
  let vptr' = castPtr vptr
      n' = fromIntegral n
  castPtr <$> [C.exp| void** { TCOD_list_insert_before($(void* l), $(void* vptr'), $(int n')) } |]

-- | Insert an element in the list
listInsertBefore :: Storable a => TCODList a -> a -> Int -> IO (Ptr (Ptr a))
listInsertBefore l v n = do
  vptr <- new v
  listInsertBeforePtr l vptr n

-- | Checking if a list is empty
listIsEmpty :: TCODList a -> IO Bool
listIsEmpty (TCODList l) = toBool <$> [C.exp| int { (int)TCOD_list_is_empty($(void* l))}|]

-- | Load to Haskell list O(N)
listToList :: forall a . Storable a => TCODList a -> IO [a]
listToList l = do
  begPtr <- listBeginPtr l
  endPtr <- listEndPtr l
  go [] begPtr endPtr
  where
    go acc begPtr endPtr = if begPtr == endPtr then pure acc
      else do
        v <- peek =<< peek endPtr
        let bytes = sizeOf (undefined :: a)
        go (v : acc) begPtr (plusPtr endPtr (negate bytes))

-- | Unload Haskell list to TCOD container O(N)
listFromList :: forall a f . (Foldable f, Storable a) => f a -> IO (TCODList a)
listFromList xs = do
  l <- listAllocate (length xs)
  mapM_ (listPush l) xs
  pure l

-- | Convert TCOD list to Haskell storable vector. O(1)
--
-- Note: The vector reuses memory of the list and valid until the TCOD list
-- is not deleted or reallocated (due mutation).
listToVectorUnsafe :: forall a . Storable a => TCODList a -> IO (Vector a)
listToVectorUnsafe l = do
  begPtr <- listBeginPtr l
  endPtr <- listEndPtr l
  if begPtr == nullPtr then pure mempty else do
    ptr <- newForeignPtr_ =<< peek begPtr
    pure $ V.unsafeFromForeignPtr0 ptr (minusPtr endPtr begPtr)

-- | Convert TCOD list to Haskell storable vector. O(N)
--
-- Note: The vector content is copied into haskell heap.
listToVector :: forall a . Storable a => TCODList a -> IO (Vector a)
listToVector = fmap V.convert . listToVectorUnsafe
