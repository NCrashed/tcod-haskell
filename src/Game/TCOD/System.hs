{-# LANGUAGE QuasiQuotes #-}
module Game.TCOD.System(
    systemStartup
  , systemShutdown
  , withSystem
  , systemElapsedMilli
  , systemElapsedSeconds
  , systemSleepMilli
  , systemSetFps
  , systemGetFps
  , systemGetLastFrameLength
  , systemSaveScreenshot
  , systemForceFullscreenResolution
  , systemSetRenderer
  , systemGetRenderer
  , systemGetCurrentResolution
  , systemGetFullscreenOffsets
  , systemGetCharSize
  , systemUpdateChar
  , systemClipboardSet
  , systemClipboardGet
  , systemGetSDLWindow
  , systemGetSDLRenderer
  , TCODEvent(..)
  , packTCODEvents
  , unpackTCODEvents
  , TCODEventInfo(..)
  , systemWaitForEvent
  , systemCheckForEvent
  , systemCreateDirectory
  , systemDeleteFile
  , systemDeleteDirectory
  , systemIsDirectory
  , systemGetDirectoryContent
  , systemFileExists
  , systemReadFile
  , systemWriteFile
  , TCODLibrary(..)
  , loadLibrary
  , getFunctionAddress
  , closeLibrary
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Char
import Data.Set (Set)
import Foreign
import Foreign.C
import Game.TCOD.ConsoleTypes
import Game.TCOD.Context as C
import Game.TCOD.Image
import Game.TCOD.List
import Game.TCOD.MouseTypes
import GHC.Generics
import Text.Printf

import SDL.Internal.Types (Window (..), Renderer(..))

import qualified Data.ByteString.Unsafe as BS
import qualified Data.Foldable as F
import qualified Data.Set as S

context tcodContext
verbatim "#define TCOD_SDL2"
include "libtcod_portability.h"
include "sys.h"

-- | Initialize global TCOD reference for system
systemStartup :: IO ()
systemStartup = [C.exp| void { TCOD_sys_startup() } |]

-- | Deallocate global TCOD reference for system
systemShutdown :: IO ()
systemShutdown = [C.exp| void { TCOD_sys_shutdown() } |]

-- | Run action with wrapped TCOD system startup/shutdown. Exception safe
-- and intented to be used in main function.
withSystem :: IO a -> IO a
withSystem = bracket_ systemStartup systemShutdown

-- | Get global timer in milliseconds
--
-- This function returns the number of milliseconds since the program has started.
systemElapsedMilli :: IO Word
systemElapsedMilli = fromIntegral <$> [C.exp| uint32_t { TCOD_sys_elapsed_milli() } |]

-- | Get global timer in seconds
--
-- This function returns the number of seconds since the program has started.
systemElapsedSeconds :: IO Double
systemElapsedSeconds = realToFrac <$> [C.exp| float { TCOD_sys_elapsed_seconds() } |]

-- | Pause the program
--
-- Use this function to stop the program execution for a specified number of milliseconds.
--
-- Note: all haskell threads on the same HEC will be paused too, try to use Haskell side
-- delays instead of the function.
systemSleepMilli :: Word -> IO ()
systemSleepMilli v = do
  let v' = fromIntegral v
  [C.exp| void { TCOD_sys_sleep_milli($(int v')) } |]

-- | Limit the frames per second
--
-- The setFps function allows you to limit the number of frames per second.
-- If a frame is rendered faster than expected, the 'consoleFlush' function will
-- wait so that the frame rate never exceed this value.
-- You can call this function during your game initialization.
-- You can dynamically change the frame rate. Just call this function once again.
-- <b>You should always limit the frame rate, except during benchmarks, else your
-- game will use 100% of the CPU power</b>
systemSetFps :: Int -> IO ()
systemSetFps v = do
  let v' = fromIntegral v
  [C.exp| void { TCOD_sys_set_fps($(int v')) } |]

-- | Get the number of frames rendered during the last second
--
-- The value returned by this function is updated every second.
systemGetFps :: IO Int
systemGetFps = fromIntegral <$> [C.exp| int { TCOD_sys_get_fps() } |]

-- | Get the duration of the last frame
--
-- This function returns the length in seconds of the last rendered frame.
-- You can use this value to update every time dependent object in the world.
systemGetLastFrameLength :: IO Float
systemGetLastFrameLength = realToFrac <$> [C.exp| float { TCOD_sys_get_last_frame_length() } |]

-- | This function allows you to save the current game screen in a png file,
-- or possibly a bmp file if you provide a filename ending with .bmp.
systemSaveScreenshot :: Maybe FilePath
  -- ^ Name of the file. If empty, a filename is automatically generated with
  -- the form "./screenshotNNN.png", NNN being the first free number (if a file
  -- named screenshot000.png already exist, screenshot001.png will be used, and so on...).
  -> IO ()
systemSaveScreenshot mp = case mp of
  Nothing -> [C.exp| void { TCOD_sys_save_screenshot(NULL) }|]
  Just p -> withCString p $ \p' -> [C.exp| void { TCOD_sys_save_screenshot($(const char* p')) }|]

-- | Using a custom resolution for the fullscreen mode
--
-- This function allows you to force the use of a specific resolution in fullscreen mode.
-- The default resolution depends on the root console size and the font character size.
--
-- Will use the smallest available resolution so that :
-- resolution width >= width and resolution width >= root console width * font char width
-- resolution width >= height and resolution height >= root console height * font char height
systemForceFullscreenResolution :: Int -- ^ width Resolution to use when switching to fullscreen.
  -> Int -- ^ height Resolution to use when switching to fullscreen.
  -> IO ()
systemForceFullscreenResolution w h = do
  let w' = fromIntegral w
      h' = fromIntegral h
  [C.exp| void { TCOD_sys_force_fullscreen_resolution($(int w'), $(int h')) } |]

-- | Dynamically change libtcod's internal renderer
--
-- As of 1.5.1, libtcod contains 3 different renderers :
-- * SDL : historic libtcod renderer. Should work and be pretty fast everywhere
-- * OpenGL : requires OpenGL compatible video card. Might be much faster or much slower than SDL, depending on the drivers
-- * GLSDL : requires OpenGL 1.4 compatible video card with GL_ARB_shader_objects extension. Blazing fast if you have the proper hardware and drivers.
-- This function switches the current renderer dynamically.
systemSetRenderer :: TCODRenderer -> IO ()
systemSetRenderer r = do
  let r' = fromIntegral . fromEnum $ r
  [C.exp| void { TCOD_sys_set_renderer((TCOD_renderer_t)$(int r')) } |]

-- | Get the current internal renderer
systemGetRenderer :: IO TCODRenderer
systemGetRenderer = toEnum . fromIntegral <$> [C.exp| int { (int)TCOD_sys_get_renderer() } |]

-- | Get current resolution
--
-- You can get the current screen resolution with getCurrentResolution. You can
-- use it for example to get the desktop resolution before initializing the root console.
systemGetCurrentResolution :: IO (Int, Int)
systemGetCurrentResolution = alloca $ \wp -> alloca $ \hp -> do
  [C.exp| void { TCOD_sys_get_current_resolution($(int* wp), $(int* hp)) } |]
  let pk = fmap fromIntegral . peek
  (,) <$> pk wp <*> pk hp

-- | Get fullscreen offset
--
-- If the fullscreen resolution does not matches the console size in pixels,
-- black borders are added. This function returns the position in pixels of the
-- console top left corner in the screen.
systemGetFullscreenOffsets :: IO (Int, Int)
systemGetFullscreenOffsets = alloca $ \wp -> alloca $ \hp -> do
  [C.exp| void { TCOD_sys_get_fullscreen_offsets($(int* wp), $(int* hp)) } |]
  let pk = fmap fromIntegral . peek
  (,) <$> pk wp <*> pk hp

-- | Get the font size
--
-- You can get the size of the characters in the font
systemGetCharSize :: IO (Int, Int)
systemGetCharSize = alloca $ \wp -> alloca $ \hp -> do
  [C.exp| void { TCOD_sys_get_char_size($(int* wp), $(int* hp)) } |]
  let pk = fmap fromIntegral . peek
  (,) <$> pk wp <*> pk hp

-- | Dynamically updating the font bitmap
--
-- You can dynamically change the bitmap of a character in the font. All cells
-- using this ascii code will be updated at next flush call.
systemUpdateChar :: Char -- ^ ascii code corresponding to the character to update
  -> Int -- ^ font x coordinate of the character in the bitmap font (in characters, not pixels)
  -> Int -- ^ font y coordinate of the character in the bitmap font (in characters, not pixels)
  -> TCODImage  -- ^ img image containing the new character bitmap
  -> Int -- ^ x position in pixels of the top-left corner of the character in the image
  -> Int -- ^ y position in pixels of the top-left corner of the character in the image
  -> IO ()
systemUpdateChar c fontX fontY (TCODImage i) x y = do
  let c' = fromIntegral . ord $ c
      fontX' = fromIntegral fontX
      fontY' = fromIntegral fontY
      x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void { TCOD_sys_update_char($(int c'), $(int fontX'), $(int fontY'), $(void* i), $(int x'), $(int y')) } |]

-- | Set current clipboard contents
--
-- Takes UTF-8 text and copies it into the system clipboard.  On Linux, because
-- an application cannot access the system clipboard unless a window is open,
-- if no window is open the call will do nothing.
systemClipboardSet :: String -> IO Bool
systemClipboardSet str = withCString str $ \str' ->
  toBool <$> [C.exp| int { (int)TCOD_sys_clipboard_set($(const char* str'))} |]

-- | Get current clipboard contents\
--
-- Returns the UTF-8 text currently in the system clipboard.
-- On Linux, because an application cannot access the system clipboard unless a
-- window is open, if no window is open an empty string will be returned.
systemClipboardGet :: IO String
systemClipboardGet = peekCString =<< [C.exp| const char* { TCOD_sys_clipboard_get() }|]

-- | Returm reference to SDL window
systemGetSDLWindow :: IO Window
systemGetSDLWindow = Window <$> [C.exp| void* { TCOD_sys_get_SDL_window() }|]

-- | Return reference to SDL renderer
systemGetSDLRenderer :: IO Renderer
systemGetSDLRenderer = Renderer <$> [C.exp| void* { TCOD_sys_get_SDL_renderer() } |]

-- | Flags to distinguish supported events
data TCODEvent =
    EventNone
  | EventKeyPress
  | EventKeyRelease
  | EventKey
  | EventMouseMove
  | EventMousePress
  | EventMouseRelease
  | EventMouse
  | EventFingerMove
  | EventFingerPress
  | EventFingerRelease
  | EventFinger
  | EventAny
  deriving (Eq, Ord, Show, Generic)

-- | Note that 'toEnum' can only capture single event
instance Enum TCODEvent where
  fromEnum v = case v of
    EventNone -> 0
    EventKeyPress -> 1
    EventKeyRelease -> 2
    EventKey -> fromEnum EventKeyPress .|. fromEnum EventKeyRelease
    EventMouseMove -> 4
    EventMousePress -> 8
    EventMouseRelease -> 16
    EventMouse -> fromEnum EventMouseMove .|. fromEnum EventMousePress .|. fromEnum EventMouseRelease
    EventFingerMove -> 32
    EventFingerPress -> 64
    EventFingerRelease -> 128
    EventFinger -> fromEnum EventFingerMove .|. fromEnum EventFingerPress .|. fromEnum EventFingerRelease
    EventAny -> fromEnum EventKey .|. fromEnum EventMouse .|. fromEnum EventFinger
  toEnum i
    | i == 0 = EventNone
    | i `contains` (4 .|. 8 .|. 16 .|. 32 .|. 64 .|. 128) = EventAny
    | i `contains` (1 .|. 2) = EventKey
    | i `contains` 1 = EventKeyPress
    | i `contains` 2 = EventKeyRelease
    | i `contains` (4 .|. 8 .|. 16) = EventMouse
    | i `contains` 4 = EventMouseMove
    | i `contains` 8 = EventMousePress
    | i `contains` 16 = EventMouseRelease
    | i `contains` (32 .|. 64 .|. 128) = EventFinger
    | otherwise = EventNone
    where
      contains v flags = (v .&. flags) /= 0

-- | Packing event flags into bitfield
packTCODEvents :: Foldable f => f TCODEvent -> Int
packTCODEvents = F.foldl' (\acc v -> acc .|. fromEnum v) 0

-- | Unpack events flags from bitfield. Note that 'EventKey', 'EventMouse' and 'EventAny' are in the set
-- if any of dependent event is occured. 'EventNone' is never occur in the set (empty set if no events)
-- and 'EventAny' is member of the set if any event occurs.
unpackTCODEvents :: Int -> Set TCODEvent
unpackTCODEvents i = F.foldl' (\acc v -> addIf (i `contains` v) v acc) mempty
  [ EventKeyPress
  , EventKeyRelease
  , EventKey
  , EventMouseMove
  , EventMousePress
  , EventMouseRelease
  , EventMouse
  , EventFingerMove
  , EventFingerPress
  , EventFingerRelease
  , EventFinger
  , EventAny ]
  where
    addIf b v = if b then S.insert v else id
    contains v flags = (v .&. fromEnum flags) /= 0

-- | Collected info about occured events in TCOD
data TCODEventInfo = TCODEventInfo {
  tcodKey    :: TCODKey       -- ^ Keyboard events
, tcodMouse  :: TCODMouse     -- ^ Mouse events
, tcodEvents :: Set TCODEvent -- ^ Set of occured event types
} deriving (Generic)

-- | Waiting for any event (mouse or keyboard)
--
-- This function waits for an event from the user. The eventMask shows what events we're waiting for.
-- The return value indicate what event was actually triggered. Values in key and mouse structures are updated accordingly.
-- If flush is false, the function waits only if there are no pending events, else it returns the first event in the buffer.
systemWaitForEvent :: Foldable f => f TCODEvent -- ^ event types to wait for (other types are discarded)
  -> Bool -- ^ Flush  if true, all pending events are flushed from the buffer. Else, return the first available event
  -> IO TCODEventInfo
systemWaitForEvent es flush = alloca $ \kp -> alloca $ \mp -> do
  let es' = fromIntegral . packTCODEvents $ es
      flush' = fromBool flush
      kp' = castPtr kp
      mp' = castPtr mp
  events <- unpackTCODEvents . fromIntegral <$> [C.exp| int { (int)TCOD_sys_wait_for_event($(int es'), (TCOD_key_t*)$(void* kp'), (TCOD_mouse_t*)$(void* mp'), $(int flush')!=0) } |]
  key <- peek kp
  mouse <- peek mp
  pure $ TCODEventInfo key mouse events

-- | Checking for any event (mouse or keyboard)
--
-- This function checks if an event from the user is in the buffer. The eventMask shows what events we're waiting for.
-- The return value indicate what event was actually found. Values in key and mouse structures are updated accordingly.
systemCheckForEvent :: Foldable f => f TCODEvent -- ^ event types to wait for (other types are discarded)
  -> IO TCODEventInfo
systemCheckForEvent es = alloca $ \kp -> alloca $ \mp -> do
  let es' = fromIntegral . packTCODEvents $ es
      kp' = castPtr kp
      mp' = castPtr mp
  events <- unpackTCODEvents . fromIntegral <$> [C.exp| int { (int)TCOD_sys_check_for_event($(int es'), (TCOD_key_t*)$(void* kp'), (TCOD_mouse_t*)$(void* mp')) } |]
  key <- peek kp
  mouse <- peek mp
  pure $ TCODEventInfo key mouse events

-- | Create a directory
--
--  All those functions return false if an error occurred.
systemCreateDirectory :: FilePath -- ^ Directory path. The immediate father directory (<path>/..) must exist and be writable.
  -> IO Bool
systemCreateDirectory p = withCString p $ \p' ->
  toBool <$> [C.exp| int {(int)TCOD_sys_create_directory($(const char* p'))} |]

-- | Delete a file
--
--  All those functions return false if an error occurred.
systemDeleteFile :: FilePath -- ^ File path. This file must exist and be writable.
  -> IO Bool
systemDeleteFile p = withCString p $ \p' ->
  toBool <$> [C.exp| int {(int)TCOD_sys_delete_file($(const char* p'))} |]

-- | Delete an empty directory
--
--  All those functions return false if an error occurred.
systemDeleteDirectory :: FilePath -- ^ Directory path. This directory must exist, be writable and empty
  -> IO Bool
systemDeleteDirectory p = withCString p $ \p' ->
  toBool <$> [C.exp| int {(int)TCOD_sys_delete_directory($(const char* p'))} |]

-- | Check if a path is a directory
--
--  All those functions return false if an error occurred.
systemIsDirectory :: FilePath -- ^ a path to check
  -> IO Bool
systemIsDirectory p = withCString p $ \p' ->
  toBool <$> [C.exp| int {(int)TCOD_sys_is_directory($(const char* p'))} |]

-- | List files in a directory
--
-- To get the list of entries in a directory (including sub-directories, except . and ..).
-- The returned list is allocated by the function and must be deleted by you.
-- All the const char * inside must be also freed with 'listClearAndDelete'.
systemGetDirectoryContent :: FilePath -- ^ a path to check
  -> String -- ^ pattern. If empty, returns all directory entries. Else returns
            -- only entries matching the pattern. The pattern is NOT a regular
            -- expression. It can only handle one '*' wildcard.
            -- Examples : *.png, saveGame*, font*.png
  -> IO (TCODList FilePath)
systemGetDirectoryContent p pat = withCString p $ \p' -> withCString pat $ \pat' ->
  TCODList <$> [C.exp| void* {TCOD_sys_get_directory_content($(const char* p'), $(const char* pat'))} |]

-- | Check if a given file exists
--
-- In order to check whether a given file exists in the filesystem. Useful for
-- detecting errors caused by missing files.
systemFileExists :: PrintfArg r
  => FilePath -- ^ filename the file name, using printf-like formatting
  -> r -- ^ optional arguments for filename formatting
  -> IO Bool
systemFileExists p r = withCString (printf p r) $ \p' ->
  toBool <$> [C.exp| int {(int)TCOD_sys_file_exists($(const char* p'))} |]

-- | Read the content of a file into memory
--
-- This is a portable function to read the content of a file from disk or from the application apk (android).
systemReadFile :: FilePath -> IO (Maybe ByteString)
systemReadFile p = withCString p $ \p' -> alloca $ \bufptr -> alloca $ \sptr -> do
  res <- toBool <$> [C.exp| int {(int)TCOD_sys_read_file($(const char* p'), $(unsigned char** bufptr), $(size_t* sptr))}|]
  if not res then pure Nothing
    else do
      bptr <- peek bufptr
      len <- peek sptr
      Just <$> BS.unsafePackMallocCStringLen (castPtr bptr, fromIntegral len)

-- | Write the content of a memory buffer to a file
--
-- This is a portable function to write some data to a file.
systemWriteFile :: FilePath -> ByteString -> IO Bool
systemWriteFile p bs = withCString p $ \p' -> BS.unsafeUseAsCStringLen bs $ \(bufptr, len) -> do
  let bufptr' = castPtr bufptr
      len' = fromIntegral len
  toBool <$> [C.exp| int {(int)TCOD_sys_write_file($(const char* p'), $(unsigned char* bufptr'), $(size_t len'))}|]

-- | TCOD dynamic library reference
newtype TCODLibrary = TCODLibrary { unTCODLibrary :: Ptr () } deriving (Eq, Ord, Show, Generic)

-- | Dynamic load of .so or .dll library.
loadLibrary :: FilePath -> IO (Maybe TCODLibrary)
loadLibrary p = withCString p $ \p' -> do
  ptr <- [C.exp| void* { TCOD_load_library($(const char* p')) } |]
  pure $ if ptr == nullPtr then Nothing else Just (TCODLibrary ptr)

-- | Dynamic load of function from library. Not safe as type of function is not
-- checked.
getFunctionAddress :: TCODLibrary -- ^ Dynamic library reference
  -> String -- ^ Name of function to load
  -> IO (Maybe (FunPtr a)) -- ^ If succeded, then return pointer to function
getFunctionAddress (TCODLibrary l) f = withCString f $ \f' -> do
  ptr <- [C.exp| void* { TCOD_get_function_address($(void* l), $(const char* f')) }|]
  pure $ if ptr == nullPtr then Nothing else Just $ castPtrToFunPtr ptr

-- | Unload dynamic library from memory
closeLibrary :: TCODLibrary -> IO ()
closeLibrary (TCODLibrary l)
  | nullPtr == l = pure ()
  | otherwise = [C.exp| void { TCOD_close_library($(void* l)) } |]
