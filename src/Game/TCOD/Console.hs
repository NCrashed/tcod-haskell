module Game.TCOD.Console(
    consoleInitRoot
  , consoleSetWindowTitle
  , consoleSetFullScreen
  , consoleIsFullScreen
  , consoleIsWindowClosed
  , consoleHasMouseFocus
  , consoleConsoleIsActive
  , consoleSetCustomFont
  , consoleMapAsciiCodeToFont
  , consoleMapAsciiCodesToFont
  , consoleMapStringToFont
  , consoleSetDirty
  , consoleSetDefaultBackground
  , consoleSetDefaultForeground
  ) where

import Game.TCOD.Color
import Game.TCOD.ConsoleTypes
import Game.TCOD.Context as C
import Game.TCOD.Image
import Game.TCOD.List

import Data.Char (isAscii)
import Foreign
import Foreign.C

context tcodContext
verbatim "#define TCOD_SDL2"
include "console.h"

-- | Creating the game window
consoleInitRoot :: Int -- ^ w size of the console(in characters). The default font in libtcod (./terminal.png) uses 8x8 pixels characters.
                       -- You can change the font by calling TCODConsole::setCustomFont before calling initRoot.
  -> Int -- ^ h
  -> String -- ^ Title of the window. It's not visible when you are in fullscreen.
            -- Note 1 : you can dynamically change the window title with 'consoleSetWindowTitle'
  -> Bool -- ^ Fullscreen whether you start in windowed or fullscreen mode.
          -- Note 1 : you can dynamically change this mode with 'consoleSetFullscreen'
          -- Note 2 : you can get current mode with 'consoleIsFullscreen'
  -> TCODRenderer -- ^ renderer which renderer to use. Possible values are :
                  -- * RendererGLSL : works only on video cards with pixel shaders
                  -- * RendererOpenGL : works on all video cards supporting OpenGL 1.4
                  -- * RendererSDL : should work everywhere!
                  -- Note 1: if you select a renderer that is not supported by the player's machine, libtcod scan the lower renderers until it finds a working one.
                  -- Note 2: on recent video cards, GLSL results in up to 900% increase of framerates in the true color sample compared to SDL renderer.
                  -- Note 3: whatever renderer you use, it can always be overridden by the player through the libtcod.cfg file.
                  -- Note 4: you can dynamically change the renderer after calling consoleInitRoot with 'consoleSetRenderer'.
                  -- Note 5: you can get current renderer with 'consoleGetRenderer'. It might be different from the one you set in consoleInitRoot in case it's not supported on the player's computer.
  -> IO ()
consoleInitRoot w h title fullscr renderer = withCString title $ \title' -> do
  let w' = fromIntegral w
      h' = fromIntegral h
      fullscr' = fromBool fullscr
      renderer' = fromIntegral . fromEnum $ renderer
  [C.exp| void { TCOD_console_init_root($(int w'), $(int h'), $(const char* title'), $(int fullscr') != 0, (TCOD_renderer_t)$(int renderer')) } |]

-- | Changing the window title
--
-- This function dynamically changes the title of the game window.
-- Note that the window title is not visible while in fullscreen.
consoleSetWindowTitle :: String -> IO ()
consoleSetWindowTitle str = withCString str $ \str' -> [C.exp| void { TCOD_console_set_window_title($(const char* str')) } |]

-- | Switching between windowed and fullscreen modes
--
-- This function switches the root console to fullscreen or windowed mode.
-- Note that there is no predefined key combination to switch to/from fullscreen.
-- You have to do this in your own code.
consoleSetFullScreen :: Bool -> IO ()
consoleSetFullScreen v = do
  let v' = fromBool v
  [C.exp| void { TCOD_console_set_fullscreen($(int v') != 0) } |]

-- | Getting the current mode
--
-- This function returns true if the current mode is fullscreen.
consoleIsFullScreen :: IO Bool
consoleIsFullScreen =
  toBool <$> [C.exp| int { (int)TCOD_console_is_fullscreen() } |]

-- | Handling "close window" events
--
-- When you start the program, this returns false. Once a "close window" event
-- has been sent by the window manager, it will always return true. You're
-- supposed to exit cleanly the game.
consoleIsWindowClosed :: IO Bool
consoleIsWindowClosed =
  toBool <$> [C.exp| int { (int)TCOD_console_is_window_closed() } |]

-- | Check if the mouse cursor is inside the game window
--
-- Returns true if the mouse cursor is inside the game window area and the game
-- window is the active application.
consoleHasMouseFocus :: IO Bool
consoleHasMouseFocus =
  toBool <$> [C.exp| int { (int)TCOD_console_has_mouse_focus() } |]

-- | Check if the game application is active
--
-- Returns false if the game window is not the active window or is iconified.
consoleConsoleIsActive :: IO Bool
consoleConsoleIsActive =
  toBool <$> [C.exp| int { (int)TCOD_console_is_active() } |]

-- | This function allows you to use a bitmap font (png or bmp) with custom character size or layout.
--  It should be called before initializing the root console with initRoot.
--  Once this function is called, you can define your own custom mappings using mapping functions
consoleSetCustomFont :: Foldable f
  => FilePath -- ^ Name of a .bmp or .png file containing the font.
  -> f TCODFontFlag -- ^ Used to define the characters layout in the bitmap and the font type
  -> Int -- ^  Number of characters in the font (horizontal).
         -- Should be 16x16 for ASCII layouts, 32x8 for TCOD layout.
         -- But you can use any other layout.
         -- If set to 0, there are deduced from the font layout flag.
  -> Int -- ^ Number of characters in the font (vertical).
  -> IO ()
consoleSetCustomFont fontName fflags h v = withCString fontName $ \fontName' -> do
  let fflag' = fromIntegral . combineFontFlags $ fflags
      h' = fromIntegral h
      v' = fromIntegral v
  [C.exp| void { TCOD_console_set_custom_font($(const char* fontName'), $(int fflag'), $(int h'), $(int v')) } |]

-- | Mapping a single ASCII code to a character
--
-- These functions allow you to map characters in the bitmap font to ASCII codes.
-- They should be called after initializing the root console with initRoot.
-- You can dynamically change the characters mapping at any time, allowing to use several fonts in the same screen.
consoleMapAsciiCodeToFont :: Int -- ^ asciiCode ASCII code to map.
  -> Int -- ^ fontCharX Coordinate of the character in the bitmap font (in characters, not pixels).
  -> Int -- ^ fontCharY
  -> IO ()
consoleMapAsciiCodeToFont asciiCode fontCharX fontCharY = do
  let asciiCode' = fromIntegral asciiCode
      fontCharX' = fromIntegral fontCharX
      fontCharY' = fromIntegral fontCharY
  [C.exp| void { TCOD_console_map_ascii_code_to_font($(int asciiCode'), $(int fontCharX'), $(int fontCharY')) } |]

-- | Mapping consecutive ASCII codes to consecutive characters
consoleMapAsciiCodesToFont :: Int -- ^ firstAsciiCode first ASCII code to map
  -> Int -- ^ nbCodes number of consecutive ASCII codes to map
  -> Int -- ^ fontCharX coordinate of the character in the bitmap font (in characters, not pixels) corresponding to the first ASCII code
  -> Int -- ^ fontCharY
  -> IO ()
consoleMapAsciiCodesToFont asciiCode nbCodes fontCharX fontCharY = do
  let asciiCode' = fromIntegral asciiCode
      nbCodes' = fromIntegral nbCodes
      fontCharX' = fromIntegral fontCharX
      fontCharY' = fromIntegral fontCharY
  [C.exp| void { TCOD_console_map_ascii_codes_to_font($(int asciiCode'), $(int nbCodes'), $(int fontCharX'), $(int fontCharY')) } |]

-- | Mapping ASCII code from a string to consecutive characters
--
-- Note: none ascii characters in the string are ignored
consoleMapStringToFont :: String -- ^ string containing the ASCII codes to map
  -> Int -- ^ fontCharX of the character in the bitmap font (in characters, not pixels) corresponding to the first ASCII code in the string
  -> Int -- ^ fontCharY
  -> IO ()
consoleMapStringToFont str fontCharX fontCharY = withCString (filter isAscii str) $ \str' -> do
  let fontCharX' = fromIntegral fontCharX
      fontCharY' = fromIntegral fontCharY
  [C.exp| void { TCOD_console_map_string_to_font($(const char* str'), $(int fontCharX'), $(int fontCharY')) } |]

-- | Mark region of console for rerender
consoleSetDirty :: Int -- ^ x
  -> Int -- ^ y
  -> Int -- ^ w
  -> Int -- ^ h
  -> IO ()
consoleSetDirty x y w h = do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
  [C.exp| void {TCOD_console_set_dirty($(int x'), $(int y'), $(int w'), $(int h'))} |]

-- | Setting the default background color
--
-- This function changes the default background color for a console. The default
-- background color is used by several drawing functions like clear, putChar, ...
consoleSetDefaultBackground :: TCODConsole -> Color -> IO ()
consoleSetDefaultBackground (TCODConsole c) col = with col $ \col' ->
  [C.exp| void { TCOD_console_set_default_background($(void* c), *$(TCOD_color_t* col'))} |]

-- | Setting the default foreground color
--
-- This function changes the default foreground color for a console. The default
-- foreground color is used by several drawing functions like clear, putChar, ...
consoleSetDefaultForeground :: TCODConsole -> Color -> IO ()
consoleSetDefaultForeground (TCODConsole c) col = with col $ \col' ->
  [C.exp| void { TCOD_console_set_default_foreground($(void* c), *$(TCOD_color_t* col'))} |]
