{-# LANGUAGE QuasiQuotes #-}
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
  , rootConsole
  , consoleSetDefaultBackground
  , consoleSetDefaultForeground
  , consoleClear
  , consoleSetCharBackground
  , consoleSetCharForeground
  , consoleSetChar
  , consolePutChar
  , consolePutCharEx
  , consoleSetBackgroundFlag
  , consoleGetBackgroundFlag
  , consoleSetAligment
  , consoleGetAligment
  , consolePrintAscii
  , consolePrintExAscii
  , consolePrintRectAscii
  , consolePrintRectExAscii
  , consoleGetHeightRectAscii
  , consoleRect
  , consoleHLine
  , consoleVLine
  , consolePrintFrame
  , consolePrintFrame'
  , consolePrint
  , consolePrintEx
  , consolePrintRect
  , consolePrintRectEx
  , consoleGetHeightRect
  , consoleGetDefaultBackground
  , consoleGetDefaultForeground
  , consoleGetCharBackground
  , consoleGetCharForeground
  , consoleGetChar
  , consoleGetBackgroundColorImage
  , consoleGetForegroundColorImage
  , consoleSetFade
  , consoleGetFade
  , consoleGetFadingColor
  , consoleFlush
  , consoleSetColorControl
  , consoleCheckForKeyPress
  , consoleWaitForKeyPress
  , consoleIsKeyPressed
  , consoleFromFile
  , consoleLoadAsc
  , consoleLoadApf
  , consoleSaveAsc
  , consoleSaveApf
  , consoleNew
  , consoleGetWidth
  , consoleGetHeight
  , consoleSetKeyColor
  , consoleBlit
  , consoleDelete
  , consoleCredits
  , consoleCreditsReset
  , consoleCreditsRender
  , consoleFromXp
  , consoleLoadXp
  , consoleSaveXp
  , consoleListFromXp
  , consoleListSaveXp
  ) where

import Game.TCOD.Color
import Game.TCOD.ConsoleTypes
import Game.TCOD.Context as C
import Game.TCOD.Image
import Game.TCOD.List

import Data.Char (isAscii, ord, chr)
import Foreign
import Foreign.C
import Text.Printf

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

-- | TCOD uses null pointer to reference root console, we wrap it to our type
-- for ease of use and hiding the subtle implementation detail.
rootConsole :: TCODConsole
rootConsole = TCODConsole nullPtr

-- | Setting the default background color
--
-- This function changes the default background color for a console. The default
-- background color is used by several drawing functions like clear, putChar, ...
consoleSetDefaultBackground :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Color -> IO ()
consoleSetDefaultBackground (TCODConsole c) col = with col $ \col' ->
  [C.exp| void { TCOD_console_set_default_background($(void* c), *$(TCOD_color_t* col'))} |]

-- | Setting the default foreground color
--
-- This function changes the default foreground color for a console. The default
-- foreground color is used by several drawing functions like clear, putChar, ...
consoleSetDefaultForeground :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Color -> IO ()
consoleSetDefaultForeground (TCODConsole c) col = with col $ \col' ->
  [C.exp| void { TCOD_console_set_default_foreground($(void* c), *$(TCOD_color_t* col'))} |]

-- | Clearing a console
--
-- This function modifies all cells of a console :
-- * set the cell's background color to the console default background color
-- * set the cell's foreground color to the console default foreground color
-- * set the cell's ASCII code to 32 (space)
consoleClear :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> IO ()
consoleClear (TCODConsole l) = [C.exp| void { TCOD_console_clear($(void* l)) } |]

-- | Setting the background color of a cell
--
-- This function modifies the background color of a cell, leaving other
-- properties (foreground color and ASCII code) unchanged.
consoleSetCharBackground :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of the cell in the console. 0 <= x < console width
  -> Int -- ^ y 0 <= y < console height
  -> Color -- ^ col the background color to use. You can use color constants
  -> TCODBackgroundFlag -- ^ flag this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> IO ()
consoleSetCharBackground (TCODConsole l) x y c flag = with c $ \c' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      flag' = fromIntegral . fromEnum $ flag
  [C.exp| void { TCOD_console_set_char_background($(void* l), $(int x'), $(int y'), *$(TCOD_color_t* c'), (TCOD_bkgnd_flag_t)$(int flag')) } |]

-- | Setting the background color of a cell
--
-- This function modifies the background color of a cell, leaving other
-- properties (foreground color and ASCII code) unchanged.
consoleSetCharForeground :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of the cell in the console. 0 <= x < console width
  -> Int -- ^ y 0 <= y < console height
  -> Color -- ^ col the background color to use. You can use color constants
  -> IO ()
consoleSetCharForeground (TCODConsole l) x y c = with c $ \c' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void { TCOD_console_set_char_foreground($(void* l), $(int x'), $(int y'), *$(TCOD_color_t* c')) } |]

-- | Setting the ASCII code of a cell
--
-- This function modifies the ASCII code of a cell, leaving other properties (background and foreground colors) unchanged.
-- Note that since a clear console has both background and foreground colors set to black for every cell, using setchar will
-- produce black characters on black background. Use putchar instead.
consoleSetChar :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of the cell in the console. 0 <= x < console width
  -> Int -- ^ y 0 <= y < console height
  -> Char -- ^ the new ASCII code for the cell
  -> IO ()
consoleSetChar (TCODConsole l) x y c = do
  let x' = fromIntegral x
      y' = fromIntegral y
      c' = fromIntegral . ord $ c
  [C.exp| void { TCOD_console_set_char($(void* l), $(int x'), $(int y'), $(int c')) } |]

-- | Setting every property of a cell using default colors
--
-- This function modifies every property of a cell :
--  * update the cell's background color according to the console default background color (see TCOD_bkgnd_flag_t).
--  * set the cell's foreground color to the console default foreground color
--  * set the cell's ASCII code to c
consolePutChar :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of the cell in the console. 0 <= x < console width
  -> Int -- ^ y 0 <= y < console height
  -> Char -- ^ the new ASCII code for the cell
  -> TCODBackgroundFlag -- ^ flag this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> IO ()
consolePutChar (TCODConsole l) x y c flag = do
  let x' = fromIntegral x
      y' = fromIntegral y
      c' = fromIntegral . ord $ c
      flag' = fromIntegral . fromEnum $ flag
  [C.exp| void { TCOD_console_put_char($(void* l), $(int x'), $(int y'), $(int c'), (TCOD_bkgnd_flag_t)$(int flag')) } |]

-- | Setting every property of a cell using specific colors
--
-- This function modifies every property of a cell :
--   * set the cell's background color to back.
--   * set the cell's foreground color to fore.
--   * set the cell's ASCII code to c.
consolePutCharEx :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of the cell in the console. 0 <= x < console width
  -> Int -- ^ y 0 <= y < console height
  -> Char -- ^ the new ASCII code for the cell
  -> Color -- ^ foreground, new foreground and background colors for this cell
  -> Color -- ^ background, new foreground and background colors for this cell
  -> IO ()
consolePutCharEx (TCODConsole l) x y c fc bc = with fc $ \fc' -> with bc $ \bc' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      c' = fromIntegral . ord $ c
  [C.exp| void { TCOD_console_put_char_ex($(void* l), $(int x'), $(int y'), $(int c'), *$(TCOD_color_t* fc'), *$(TCOD_color_t* bc')) } |]

-- | Setting the default background flag
--
-- This function defines the background mode (see TCOD_bkgnd_flag_t) for the console.
-- This default mode is used by several functions (print, printRect, ...)
consoleSetBackgroundFlag :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> TCODBackgroundFlag
  -> IO ()
consoleSetBackgroundFlag (TCODConsole l) v = do
  let v' = fromIntegral . fromEnum $ v
  [C.exp| void { TCOD_console_set_background_flag($(void* l), (TCOD_bkgnd_flag_t)$(int v'))}|]

-- | Getting the default background flag
--
-- This function returns the background mode (see TCOD_bkgnd_flag_t) for the console.
-- This default mode is used by several functions (print, printRect, ...)
consoleGetBackgroundFlag :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> IO TCODBackgroundFlag
consoleGetBackgroundFlag (TCODConsole l) = toEnum . fromIntegral <$>
  [C.exp| int { (int)TCOD_console_get_background_flag($(void* l)) } |]

-- | Setting the default alignment
--
-- This function defines the default alignment (see TCOD_alignment_t) for the console.
-- This default alignment is used by several functions (print, printRect, ...).
consoleSetAligment :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> TCODAlignment
  -> IO ()
consoleSetAligment (TCODConsole l) v = do
  let v' = fromIntegral . fromEnum $ v
  [C.exp| void { TCOD_console_set_alignment($(void* l), (TCOD_alignment_t)$(int v'))}|]

-- | Getting the default alignment
--
-- This function returns the default alignment (see TCOD_alignment_t) for the console.
-- This default mode is used by several functions (print, printRect, ...).
consoleGetAligment :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> IO TCODAlignment
consoleGetAligment (TCODConsole l) = toEnum . fromIntegral <$>
  [C.exp| int { (int)TCOD_console_get_alignment($(void* l)) } |]

-- | Printing a string with default parameters
--
-- This function print a string at a specific position using current default
-- alignment, background flag, foreground and background colors.
--
-- Note: works same as 'Text.Printf' functions
consolePrintAscii :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO ()
consolePrintAscii (TCODConsole l) x y fmt r = withCString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void { TCOD_console_print($(void* l), $(int x'), $(int y'), $(const char* fmt')) } |]

-- | Printing a string with specific alignment and background mode
--
-- his function print a string at a specific position using specific alignment
-- and background flag, but default foreground and background colors.
--
-- Note: works same as 'Text.Printf' functions
consolePrintExAscii :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> TCODBackgroundFlag -- ^ this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> TCODAlignment -- ^ aligment  defines how the strings are printed on screen.
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO ()
consolePrintExAscii (TCODConsole l) x y bf al fmt r = withCString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      bf' = fromIntegral . fromEnum $ bf
      al' = fromIntegral . fromEnum $ al
  [C.exp| void { TCOD_console_print_ex($(void* l), $(int x'), $(int y'), (TCOD_bkgnd_flag_t)$(int bf'), (TCOD_alignment_t)$(int al'), $(const char* fmt')) } |]

-- |  Printing a string with default parameters and autowrap
--
-- This function draws a string in a rectangle inside the console, using default colors, alignment and background mode.
-- If the string reaches the borders of the rectangle, carriage returns are inserted.
-- If h > 0 and the bottom of the rectangle is reached, the string is truncated. If h = 0, the string is only truncated if it reaches the bottom of the console.
-- The function returns the height (number of console lines) of the printed string.
--
-- Note: works same as 'Text.Printf' functions
consolePrintRectAscii :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ w size of the rectangle. x <= x+w < console width
  -> Int -- ^ y size of the rectangle. y <= y+h < console height
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO Int
consolePrintRectAscii (TCODConsole l) x y w h fmt r = withCString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
  fromIntegral <$> [C.exp| int { TCOD_console_print_rect($(void* l), $(int x'), $(int y'), $(int w'), $(int h'), $(const char* fmt')) } |]

-- | Printing a string with specific alignment and background mode and autowrap
--
-- This function draws a string in a rectangle inside the console, using default colors, but specific alignment and background mode.
-- If the string reaches the borders of the rectangle, carriage returns are inserted.
-- If h > 0 and the bottom of the rectangle is reached, the string is truncated. If h = 0, the string is only truncated if it reaches the bottom of the console.
-- The function returns the height (number of console lines) of the printed string.
--
-- Note: works same as 'Text.Printf' functions
consolePrintRectExAscii :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ w size of the rectangle. x <= x+w < console width
  -> Int -- ^ y size of the rectangle. y <= y+h < console height
  -> TCODBackgroundFlag -- ^ this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> TCODAlignment -- ^ aligment  defines how the strings are printed on screen.
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO Int
consolePrintRectExAscii (TCODConsole l) x y w h bf al fmt r = withCString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
      bf' = fromIntegral . fromEnum $ bf
      al' = fromIntegral . fromEnum $ al
  fromIntegral <$> [C.exp| int { TCOD_console_print_rect_ex($(void* l), $(int x'), $(int y'), $(int w'), $(int h'), (TCOD_bkgnd_flag_t)$(int bf'), (TCOD_alignment_t)$(int al'), $(const char* fmt')) } |]

-- | Compute the height of an autowrapped string
--
-- This function returns the expected height of an autowrapped string without actually printing the string with printRect or printRectEx
--
-- Note: works same as 'Text.Printf' functions
consoleGetHeightRectAscii :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ w size of the rectangle. x <= x+w < console width
  -> Int -- ^ y size of the rectangle. y <= y+h < console height
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO Int
consoleGetHeightRectAscii (TCODConsole l) x y w h fmt r = withCString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
  fromIntegral <$> [C.exp| int { TCOD_console_get_height_rect($(void* l), $(int x'), $(int y'), $(int w'), $(int h'), $(const char* fmt')) } |]

-- | Filling a rectangle with the background color
--
-- Fill a rectangle inside a console. For each cell in the rectangle :
-- * set the cell's background color to the console default background color
-- * if clear is true, set the cell's ASCII code to 32 (space)
consoleRect :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of rectangle upper-left corner in the console. 0 <= x < console width
  -> Int -- ^ y coordinates of rectangle upper-left corner in the console. 0 <= y < console height
  -> Int -- ^ w size of the rectangle in the console. x <= x+w < console width
  -> Int -- ^ h size of the rectangle in the console. y <= y+h < console height
  -> Bool -- ^ clear if true, all characters inside the rectangle are set to ASCII code 32 (space). If false, only the background color is modified
  -> TCODBackgroundFlag -- ^ flag this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> IO ()
consoleRect (TCODConsole l) x y w h clear flag = do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
      clear' = fromBool clear
      flag' = fromIntegral . fromEnum $ flag
  [C.exp| void { TCOD_console_rect($(void* l), $(int x'), $(int y'), $(int w'), $(int h'), $(int clear')!=0, (TCOD_bkgnd_flag_t)$(int flag')) } |]

-- | Drawing an horizontal line
--
-- Draws an horizontal line in the console, using ASCII code
-- TCOD_CHAR_HLINE (196), and the console's default background/foreground colors.
consoleHLine :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of rectangle upper-left corner in the console. 0 <= x < console width
  -> Int -- ^ y coordinates of rectangle upper-left corner in the console. 0 <= y < console height
  -> Int -- ^ l The length of the line in cells 1 <= l <= console width - x
  -> TCODBackgroundFlag -- ^ flag this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> IO ()
consoleHLine (TCODConsole l) x y lv flag = do
  let x' = fromIntegral x
      y' = fromIntegral y
      lv' = fromIntegral lv
      flag' = fromIntegral . fromEnum $ flag
  [C.exp| void { TCOD_console_hline($(void* l), $(int x'), $(int y'), $(int lv'), (TCOD_bkgnd_flag_t)$(int flag')) } |]

-- | Drawing an horizontal line
--
-- Draws an horizontal line in the console, using ASCII code
-- TCOD_CHAR_HLINE (196), and the console's default background/foreground colors.
consoleVLine :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of rectangle upper-left corner in the console. 0 <= x < console width
  -> Int -- ^ y coordinates of rectangle upper-left corner in the console. 0 <= y < console height
  -> Int -- ^ l The length of the line in cells 1 <= l <= console width - x
  -> TCODBackgroundFlag -- ^ flag this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> IO ()
consoleVLine (TCODConsole l) x y lv flag = do
  let x' = fromIntegral x
      y' = fromIntegral y
      lv' = fromIntegral lv
      flag' = fromIntegral . fromEnum $ flag
  [C.exp| void { TCOD_console_vline($(void* l), $(int x'), $(int y'), $(int lv'), (TCOD_bkgnd_flag_t)$(int flag')) } |]

-- | Drawing a window frame
--
-- This function calls the rect function using the supplied background mode flag,
-- then draws a rectangle with the console's default foreground color. fmt is
-- printed on the top of the rectangle, using inverted colors.
consolePrintFrame :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of rectangle upper-left corner in the console. 0 <= x < console width
  -> Int -- ^ y coordinates of rectangle upper-left corner in the console. 0 <= y < console height
  -> Int -- ^ w size of the rectangle in the console. x <= x+w < console width
  -> Int -- ^ h size of the rectangle in the console. y <= y+h < console height
  -> Bool -- ^ clear if true, all characters inside the rectangle are set to ASCII code 32 (space).
          -- If false, only the background color is modified
  -> TCODBackgroundFlag -- ^ flag this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> String -- ^ printf-like format string, eventually followed by parameters. You can use control codes to change the colors inside the string.
  -> r
  -> IO ()
consolePrintFrame (TCODConsole l) x y w h empty flag fmt r = withCString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
      empty' = fromBool empty
      flag' = fromIntegral . fromEnum $ flag
  [C.exp| void { TCOD_console_print_frame($(void* l), $(int x'), $(int y'), $(int w'), $(int h'), $(int empty')!=0, (TCOD_bkgnd_flag_t)$(int flag'), $(const char* fmt')) } |]

-- | Drawing a window frame
--
-- This function calls the rect function using the supplied background mode flag,
-- then draws a rectangle with the console's default foreground color.
consolePrintFrame' :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of rectangle upper-left corner in the console. 0 <= x < console width
  -> Int -- ^ y coordinates of rectangle upper-left corner in the console. 0 <= y < console height
  -> Int -- ^ w size of the rectangle in the console. x <= x+w < console width
  -> Int -- ^ h size of the rectangle in the console. y <= y+h < console height
  -> Bool -- ^ clear if true, all characters inside the rectangle are set to ASCII code 32 (space).
          -- If false, only the background color is modified
  -> TCODBackgroundFlag -- ^ flag this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> IO ()
consolePrintFrame' (TCODConsole l) x y w h empty flag = do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
      empty' = fromBool empty
      flag' = fromIntegral . fromEnum $ flag
  [C.exp| void { TCOD_console_print_frame($(void* l), $(int x'), $(int y'), $(int w'), $(int h'), $(int empty')!=0, (TCOD_bkgnd_flag_t)$(int flag'), NULL) } |]

-- | Mapping unicode code from a string to consecutive characters
--
-- Those functions are similar to their ASCII equivalent, but work with unicode strings (wchar_t in C/C++).
consoleMapStringToFontUtf :: String -- ^ string containing UTF codes to map
  -> Int -- ^ fontCharX of the character in the bitmap font (in characters, not pixels) corresponding to the first UTF code in the string
  -> Int -- ^ fontCharY
  -> IO ()
consoleMapStringToFontUtf str x y = withCWString str $ \str' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void { TCOD_console_map_string_to_font_utf($(const wchar_t* str'), $(int x'), $(int y')) } |]

-- | Printing a string with default parameters, unicode version
--
-- This function print a string at a specific position using current default
-- alignment, background flag, foreground and background colors.
--
-- Those functions are similar to their ASCII equivalent, but work with unicode strings (wchar_t in C/C++).
-- Note: works same as 'Text.Printf' functions
consolePrint :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO ()
consolePrint (TCODConsole l) x y fmt r = withCWString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void { TCOD_console_print_utf($(void* l), $(int x'), $(int y'), $(const wchar_t* fmt')) } |]

-- | Printing a string with specific alignment and background mode
--
-- his function print a string at a specific position using specific alignment
-- and background flag, but default foreground and background colors.
--
-- Note: works same as 'Text.Printf' functions
consolePrintEx :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> TCODBackgroundFlag -- ^ this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> TCODAlignment -- ^ aligment  defines how the strings are printed on screen.
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO ()
consolePrintEx (TCODConsole l) x y bf al fmt r = withCWString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      bf' = fromIntegral . fromEnum $ bf
      al' = fromIntegral . fromEnum $ al
  [C.exp| void { TCOD_console_print_ex_utf($(void* l), $(int x'), $(int y'), (TCOD_bkgnd_flag_t)$(int bf'), (TCOD_alignment_t)$(int al'), $(const wchar_t* fmt')) } |]

-- |  Printing a string with default parameters and autowrap, unicode version
--
-- This function draws a string in a rectangle inside the console, using default colors, alignment and background mode.
-- If the string reaches the borders of the rectangle, carriage returns are inserted.
-- If h > 0 and the bottom of the rectangle is reached, the string is truncated. If h = 0, the string is only truncated if it reaches the bottom of the console.
-- The function returns the height (number of console lines) of the printed string.
--
-- Note: works same as 'Text.Printf' functions
consolePrintRect :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ w size of the rectangle. x <= x+w < console width
  -> Int -- ^ y size of the rectangle. y <= y+h < console height
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO Int
consolePrintRect (TCODConsole l) x y w h fmt r = withCWString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
  fromIntegral <$> [C.exp| int { TCOD_console_print_rect_utf($(void* l), $(int x'), $(int y'), $(int w'), $(int h'), $(const wchar_t* fmt')) } |]

-- | Printing a string with specific alignment and background mode and autowrap, unicode version
--
-- This function draws a string in a rectangle inside the console, using default colors, but specific alignment and background mode.
-- If the string reaches the borders of the rectangle, carriage returns are inserted.
-- If h > 0 and the bottom of the rectangle is reached, the string is truncated. If h = 0, the string is only truncated if it reaches the bottom of the console.
-- The function returns the height (number of console lines) of the printed string.
--
-- Note: works same as 'Text.Printf' functions
consolePrintRectEx :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ w size of the rectangle. x <= x+w < console width
  -> Int -- ^ y size of the rectangle. y <= y+h < console height
  -> TCODBackgroundFlag -- ^ this flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'
  -> TCODAlignment -- ^ aligment  defines how the strings are printed on screen.
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO Int
consolePrintRectEx (TCODConsole l) x y w h bf al fmt r = withCWString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
      bf' = fromIntegral . fromEnum $ bf
      al' = fromIntegral . fromEnum $ al
  fromIntegral <$> [C.exp| int { TCOD_console_print_rect_ex_utf($(void* l), $(int x'), $(int y'), $(int w'), $(int h'), (TCOD_bkgnd_flag_t)$(int bf'), (TCOD_alignment_t)$(int al'), $(const wchar_t* fmt')) } |]

-- | Compute the height of an autowrapped string, utf version
--
-- This function returns the expected height of an autowrapped string without actually printing the string with printRect or printRectEx
--
-- Note: works same as 'Text.Printf' functions
consoleGetHeightRect :: PrintfArg r
  => TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ y coordinate of the character in the console, depending on the default alignment for this console
  -> Int -- ^ w size of the rectangle. x <= x+w < console width
  -> Int -- ^ y size of the rectangle. y <= y+h < console height
  -> String -- ^ Formating string, see 'Text.Printf'
  -> r
  -> IO Int
consoleGetHeightRect (TCODConsole l) x y w h fmt r = withCWString (printf fmt r) $ \fmt' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
  fromIntegral <$> [C.exp| int { TCOD_console_get_height_rect_utf($(void* l), $(int x'), $(int y'), $(int w'), $(int h'), $(const wchar_t* fmt')) } |]

-- | Reading the default background color
--
-- This function returns the default background color of a console.
consoleGetDefaultBackground :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> IO Color
consoleGetDefaultBackground (TCODConsole l) = alloca $ \res -> do
  [C.exp| void { *$(TCOD_color_t* res) = TCOD_console_get_default_background($(void* l))} |]
  peek res

-- | Reading the default foreground color
--
-- This function returns the default foreground color of a console.
consoleGetDefaultForeground :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> IO Color
consoleGetDefaultForeground (TCODConsole l) = alloca $ \res -> do
  [C.exp| void { *$(TCOD_color_t* res) = TCOD_console_get_default_foreground($(void* l))} |]
  peek res

-- | Reading the background color of a cell
--
-- This function returns the background color of a cell.
consoleGetCharBackground :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of the cell in the console. 0 <= x < console width
  -> Int -- ^ y coordinates of the cell in the console. 0 <= y < console height
  -> IO Color
consoleGetCharBackground (TCODConsole l) x y = alloca $ \res -> do
  let x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void { *$(TCOD_color_t* res) = TCOD_console_get_char_background($(void* l), $(int x'), $(int y'))} |]
  peek res

-- | Reading the foreground color of a cell
--
-- This function returns the foreground color of a cell.
consoleGetCharForeground :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of the cell in the console. 0 <= x < console width
  -> Int -- ^ y coordinates of the cell in the console. 0 <= y < console height
  -> IO Color
consoleGetCharForeground (TCODConsole l) x y = alloca $ \res -> do
  let x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void { *$(TCOD_color_t* res) = TCOD_console_get_char_foreground($(void* l), $(int x'), $(int y'))} |]
  peek res

-- | Reading the ASCII code of a cell
consoleGetChar :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Int -- ^ x coordinates of the cell in the console. 0 <= x < console width
  -> Int -- ^ y coordinates of the cell in the console. 0 <= y < console height
  -> IO Char
consoleGetChar (TCODConsole l) x y = do
  let x' = fromIntegral x
      y' = fromIntegral y
  chr . fromIntegral <$> [C.exp| int { TCOD_console_get_char($(void* l), $(int x'), $(int y'))} |]

-- | Manipulating background colors as an image
--
-- This function obtains the image containing the console background colors.
consoleGetBackgroundColorImage :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> IO TCODImage
consoleGetBackgroundColorImage (TCODConsole l) = TCODImage <$> [C.exp| void* { TCOD_console_get_background_color_image($(void* l)) }|]

-- | Manipulating foreground colors as an image
--
-- This function obtains the image containing the console foreground colors.
consoleGetForegroundColorImage :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> IO TCODImage
consoleGetForegroundColorImage (TCODConsole l) = TCODImage <$> [C.exp| void* { TCOD_console_get_foreground_color_image($(void* l)) }|]

-- | Changing the fading parameters
--
-- This function defines the fading parameters, allowing to easily fade the
-- game screen to/from a color. Once they are defined, the fading parameters
-- are valid for ever. You don't have to call setFade for each rendered frame
-- (unless you change the fading parameters).
consoleSetFade :: Word8 -- ^ fade the fading amount. 0 => the screen is filled with the fading color. 255 => no fading effect
  -> Color -- ^ the color to use during the console flushing operation
  -> IO ()
consoleSetFade v c = with c $ \c' -> do
  let v' = fromIntegral v
  [C.exp| void { TCOD_console_set_fade($(unsigned char v'), *$(TCOD_color_t* c')) } |]

-- | Reading the fade amount
--
-- This function returns the current fade amount, previously defined by 'setFade'.
consoleGetFade :: IO Word8
consoleGetFade = fromIntegral <$> [C.exp| unsigned char { TCOD_console_get_fade() } |]

-- | Reading the fading color
--
-- This function returns the current fading color, previously defined by setFade.
consoleGetFadingColor :: IO Color
consoleGetFadingColor = alloca $ \c' -> do
  [C.exp| void { *$(TCOD_color_t* c') = TCOD_console_get_fading_color() } |]
  peek c'

-- | Once the root console is initialized, you can use one of the printing
-- functions to change the background colors, the foreground colors or the
-- ASCII characters on the console.
--
-- Once you've finished rendering the root console, you have to actually apply
-- the updates to the screen with this function.
consoleFlush :: IO ()
consoleFlush = [C.exp| void { TCOD_console_flush() } |]

-- | Changing the colors while printing a string
--
-- If you want to draw a string using different colors for each word, the basic
-- solution is to call a string printing function several times, changing the
-- default colors between each call.
--
-- The TCOD library offers a simpler way to do this, allowing you to draw a
-- string using different colors in a single call. For this, you have to insert
-- color control codes in your string.
--
-- A color control code is associated with a color set (a foreground color and
-- a background color). If you insert this code in your string, the next
-- characters will use the colors associated with the color control code.
--
-- There are 5 predefined color control codes :
--   'Ctrl_1'
--   'Ctrl_2'
--   'Ctrl_3'
--   'Ctrl_4'
--   'Ctrl_5'
-- To associate a color with a code, use 'consoleSetColorControl'.
-- To go back to the console's default colors, insert in your string the color stop control code :
--   'CtrlStop'
--
-- You can also use any color without assigning it to a control code, using the generic control codes :
--   'CtrlForeRgb'
--   'CtrlBackRgb'
--
--  Those controls respectively change the foreground and background color used to print the string characters. In the string, you must insert the r,g,b components of the color (between 1 and 255. The value 0 is forbidden because it represents the end of the string in C/C++) immediately after this code.
consoleSetColorControl :: TCODColorControl
  -> Color -- ^ foreground
  -> Color -- ^ background
  -> IO ()
consoleSetColorControl ctrl cf cb = with cf $ \cf' -> with cb $ \cb' -> do
  let ctrl' = fromIntegral . fromEnum $ ctrl
  [C.exp| void { TCOD_console_set_color_control((TCOD_colctrl_t)$(int ctrl'), *$(TCOD_color_t* cf'), *$(TCOD_color_t* cb')) } |]

-- | Deprecated as of 1.5.1
consoleCheckForKeyPress :: TCODKeyStatus -- ^ flags
  -> IO TCODKey
consoleCheckForKeyPress flags = alloca $ \res -> do
  let flags' = fromIntegral . fromEnum $ flags
      res' = castPtr res
  [C.exp| void { *((TCOD_key_t*)$(void* res')) = TCOD_console_check_for_keypress($(int flags'))}|]
  peek res
{-# DEPRECATED consoleCheckForKeyPress "is deprecated as of 1.5.1" #-}

-- | Deprecated as of 1.5.1
consoleWaitForKeyPress :: Bool -- ^ flush
  -> IO TCODKey
consoleWaitForKeyPress flush = alloca $ \res -> do
  let flush' = fromBool flush
      res' = castPtr res
  [C.exp| void { *((TCOD_key_t*)$(void* res')) = TCOD_console_wait_for_keypress($(int flush') != 0)}|]
  peek res
{-# DEPRECATED consoleWaitForKeyPress "is deprecated as of 1.5.1" #-}

-- | The preferred way to check for user input is to use checkForEvent below,
-- but you can also get the status of any special key at any time with the function
consoleIsKeyPressed :: TCODKeyCode -> IO Bool
consoleIsKeyPressed kc = do
  let kc' = fromIntegral . fromEnum $ kc
  toBool <$> [C.exp| int { (int)TCOD_console_is_key_pressed((TCOD_keycode_t)$(int kc')) } |]

-- | Creating an offscreen console from a .asc or .apf file
--
-- You can create an offscreen console from a file created with Ascii Paint with this constructor
consoleFromFile :: FilePath
  -> IO TCODConsole
consoleFromFile p = withCString p $ \p' ->
  TCODConsole <$> [C.exp| void* { TCOD_console_from_file($(const char* p')) } |]

-- | Loading an offscreen console from a .asc file
--
-- You can load data from a file created with Ascii Paint with this function.
-- When needed, the console will be resized to fit the file size. The function
-- returns false if it couldn't read the file.
consoleLoadAsc :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> FilePath -- ^ path to the .asc file created with Ascii Paint
  -> IO Bool
consoleLoadAsc (TCODConsole l) p = withCString p $ \p' ->
  toBool <$> [C.exp| int { (int)TCOD_console_load_asc($(void* l), $(const char* p')) } |]

-- | Loading an offscreen console from a .apf file
--
-- You can load data from a file created with Ascii Paint with this function.
-- When needed, the console will be resized to fit the file size. The function
-- returns false if it couldn't read the file.
consoleLoadApf :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> FilePath -- ^ path to the .apf file created with Ascii Paint
  -> IO Bool
consoleLoadApf (TCODConsole l) p = withCString p $ \p' ->
  toBool <$> [C.exp| int { (int)TCOD_console_load_apf($(void* l), $(const char* p')) } |]

-- | Saving a console to a .asc file
--
-- You can save data from a console to Ascii Paint format with this function.
-- The function returns false if it couldn't write the file. This is the only
-- ASC function that works also with the root console !
consoleSaveAsc :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> FilePath -- ^ path to the .asc file created with Ascii Paint
  -> IO Bool
consoleSaveAsc (TCODConsole l) p = withCString p $ \p' ->
  toBool <$> [C.exp| int { (int)TCOD_console_save_asc($(void* l), $(const char* p')) } |]

-- | Saving a console to a .apf file
--
-- You can save data from a console to Ascii Paint format with this function.
-- The function returns false if it couldn't write the file. This is the only
-- ASC function that works also with the root console !
consoleSaveApf :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> FilePath -- ^ path to the .apf file created with Ascii Paint
  -> IO Bool
consoleSaveApf (TCODConsole l) p = withCString p $ \p' ->
  toBool <$> [C.exp| int { (int)TCOD_console_save_apf($(void* l), $(const char* p')) } |]

-- | Creating an offscreen console
--
-- You can create as many off-screen consoles as you want by using this function.
-- You can draw on them as you would do with the root console, but you cannot
-- flush them to the screen. Else, you can blit them on other consoles, including
-- the root console. See blit. The C version of this function returns a console
-- handler that you can use in most console drawing functions.
consoleNew :: Int -- ^ w the console size. 0 < w
  -> Int -- ^ h the console size. 0 < h
  -> IO TCODConsole
consoleNew w h = do
  let w' = fromIntegral w
      h' = fromIntegral h
  TCODConsole <$> [C.exp| void* { TCOD_console_new($(int w'), $(int h'))} |]

-- | Get the console's width
--
-- This function returns the width of a console (either the root console or an offscreen console)
consoleGetWidth :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> IO Int
consoleGetWidth (TCODConsole l) = fromIntegral <$> [C.exp| int {TCOD_console_get_width($(void* l))}|]

-- | Get the console's height
--
-- This function returns the height of a console (either the root console or an offscreen console)
consoleGetHeight :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> IO Int
consoleGetHeight (TCODConsole l) = fromIntegral <$> [C.exp| int {TCOD_console_get_height($(void* l))}|]

-- | Define a blit-transparent color
--
-- This function defines a transparent background color for an offscreen console.
-- All cells with this background color are ignored by the blit operation.
-- You can use it to blit only some parts of the console.
consoleSetKeyColor :: TCODConsole -- ^ the offscreen console handler or NULL for the root console
  -> Color -- ^ the transparent background color
  -> IO ()
consoleSetKeyColor (TCODConsole l) c = with c $ \c' ->
  [C.exp| void { TCOD_console_set_key_color($(void* l), *$(TCOD_color_t* c'))} |]

-- | Blitting a console on another one
--
-- This function allows you to blit a rectangular area of the source console at
-- a specific position on a destination console. It can also simulate alpha
-- transparency with the fade parameter.
consoleBlit :: TCODConsole -- ^ The source console that must be blitted on another one.
  -> Int -- ^ x src. The rectangular area of the source console that will be blitted.
         -- If wSrc and/or hSrc == 0, the source console width/height are used
  -> Int -- ^ y src
  -> Int -- ^ w src
  -> Int -- ^ h src
  -> TCODConsole -- ^ dist console. The destination console.
  -> Int -- ^ x dst. Where to blit the upper-left corner of the source area in the destination console.
  -> Int -- ^ y dst
  -> Float -- ^ foreground alpha. foregroundAlpha,backgroundAlpha Alpha transparency of the blitted console.
           -- 0.0 => The source console is completely transparent. This function does nothing.
           -- 1.0 => The source console is opaque. Its cells replace the destination cells.
           -- 0 < fade < 1.0 => The source console is partially blitted, simulating real transparency.
  -> Float -- ^ background alpha
  -> IO ()
consoleBlit (TCODConsole src) xSrc ySrc wSrc hSrc (TCODConsole dst) xDst yDst fa ba = do
  let xSrc' = fromIntegral xSrc
      ySrc' = fromIntegral ySrc
      wSrc' = fromIntegral wSrc
      hSrc' = fromIntegral hSrc
      xDst' = fromIntegral xDst
      yDst' = fromIntegral yDst
      fa' = realToFrac fa
      ba' = realToFrac ba
  [C.exp| void { TCOD_console_blit($(void* src), $(int xSrc'), $(int ySrc'), $(int wSrc'), $(int hSrc'), $(void* dst), $(int xDst'), $(int yDst'), $(float fa'), $(float ba')) } |]

-- | Destroying an offscreen console
--
-- Use this function to destroy an offscreen console
-- and release any resources allocated. Don't use it on the root console.
consoleDelete :: TCODConsole -> IO ()
consoleDelete (TCODConsole l) = [C.exp| void {TCOD_console_delete($(void* l))} |]

-- | Using a separate credit page
--
-- You can print a "Powered by libtcod x.y.z" screen during your game startup simply by calling this function after initRoot.
-- The credits screen can be skipped by pressing any key.
consoleCredits :: IO ()
consoleCredits = [C.exp| void {TCOD_console_credits()}|]

-- | Restart the credits animation
--
-- When using rederCredits, you can restart the credits animation from the
-- beginning before it's finished by calling this function.
consoleCreditsReset :: IO ()
consoleCreditsReset = [C.exp| void {TCOD_console_credits_reset()} |]

-- | Embedding credits in an existing page
--
-- You can also print the credits on one of your game screens (your main menu
-- for example) by calling this function in your main loop.
-- This function returns true when the credits screen is finished, indicating
-- that you no longer need to call it.
consoleCreditsRender :: Int -- ^ x Position of the credits text in your root console
  -> Int -- ^ y Position of the credits text in your root console
  -> Bool -- ^ alpha If true, credits are transparently added on top of the existing screen.
          -- For this to work, this function must be placed between your screen rendering code and the console flush.
  -> IO Bool
consoleCreditsRender x y a = do
  let x' = fromIntegral x
      y' = fromIntegral y
      a' = fromBool a
  toBool <$> [C.exp| int {(int)TCOD_console_credits_render($(int x'), $(int y'), $(int a')!=0)} |]

-- | REXPaint support
consoleFromXp :: FilePath -> IO TCODConsole
consoleFromXp p = withCString p $ \p' -> TCODConsole <$>
  [C.exp| void* { TCOD_console_from_xp($(const char* p')) } |]

-- | REXPaint support
consoleLoadXp :: TCODConsole -> FilePath -> IO Bool
consoleLoadXp (TCODConsole l) p = withCString p $ \p' -> toBool <$>
  [C.exp| int { (int)TCOD_console_load_xp($(void* l), $(const char* p'))}|]

-- | REXPaint support
consoleSaveXp :: TCODConsole
  -> FilePath
  -> Int -- ^ compress level
  -> IO Bool
consoleSaveXp (TCODConsole l) p c = withCString p $ \p' -> do
  let c' = fromIntegral c
  toBool <$> [C.exp| int { (int)TCOD_console_save_xp($(void* l), $(const char* p'), $(int c'))}|]

-- | REXPaint support
consoleListFromXp :: FilePath
  -> IO (TCODList TCODConsole)
consoleListFromXp p = withCString p $ \p' ->
  TCODList <$> [C.exp| void* { TCOD_console_list_from_xp($(const char* p')) } |]

-- | REXPaint support
consoleListSaveXp :: TCODList TCODConsole
  -> FilePath
  -> Int -- ^ compress level
  -> IO Bool
consoleListSaveXp (TCODList l) p c = withCString p $ \p' -> do
  let c' = fromIntegral c
  toBool <$> [C.exp| int { (int)TCOD_console_list_save_xp($(void* l), $(const char* p'), $(int c')) } |]
