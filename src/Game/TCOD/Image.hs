module Game.TCOD.Image(
    TCODImage(..)
  , imageNew
  , imageFromConsole
  , imageRefreshConsole
  , imageLoad
  , imageClear
  , imageInvert
  , imageHFlip
  , imageVFlip
  , imageRotate90
  , imageScale
  , imageSave
  , imageGetSize
  , imageGetPixel
  , imageGetAlpha
  , imageGetMipmapPixel
  , imagePutPixel
  , imageBlit
  , imageBlitRect
  , imageBlit2X
  , imageDelete
  , imageSetKeyColor
  , imageIsPixelTransparent
  ) where

import Foreign
import Foreign.C
import GHC.Generics

import Game.TCOD.Color
import Game.TCOD.ConsoleTypes
import Game.TCOD.Context as C

context tcodContext
verbatim "#define TCOD_SDL2"
include "libtcod/portability.h"
include "libtcod/image.h"

-- | TCOD image
newtype TCODImage = TCODImage { unTCODImage :: Ptr () }
  deriving (Eq, Ord, Show, Generic)

-- | You can create an image of any size, filled with black with this function.
imageNew :: Int -- ^ Width
  -> Int -- ^ Height
  -> IO TCODImage
imageNew w h = TCODImage <$> [C.exp| void* { TCOD_image_new($(int w'), $(int h')) } |]
  where w' = fromIntegral w
        h' = fromIntegral h

-- | You can create an image from any console (either the root console or an offscreen console).
-- The image size will depend on the console size and the font characters size.
-- You can then save the image to a file with the save function.
imageFromConsole :: TCODConsole -> IO TCODImage
imageFromConsole (TCODConsole c) = TCODImage <$> [C.exp| void* { TCOD_image_from_console($(void* c)) } |]

-- | If you need to refresh the image with the console's new content, you don't have to delete it and create another one. Instead, use this function. Note that you must use the same console that was used in the TCOD_image_from_console call (or at least a console with the same size).
imageRefreshConsole :: TCODImage -> TCODConsole -> IO ()
imageRefreshConsole (TCODImage i) (TCODConsole c) = [C.exp| void {TCOD_image_refresh_console($(void* i), $(void* c))} |]

-- | You can read data from a .bmp or .png file (for example to draw an image using the background color of the console cells).
-- Note that only 24bits and 32bits PNG files are currently supported.
imageLoad :: FilePath -> IO TCODImage
imageLoad str = withCString str $ \str' -> TCODImage <$> [C.exp| void* { TCOD_image_load($(const char* str')) }|]

-- | Filling an image with a color
imageClear :: TCODImage -> Color -> IO ()
imageClear (TCODImage i) c = with c $ \c' -> [C.exp| void { TCOD_image_clear($(void* i), *$(TCOD_color_t* c')) } |]

-- | Inverting the colors of the image
imageInvert :: TCODImage -> IO ()
imageInvert (TCODImage i) = [C.exp| void { TCOD_image_invert($(void* i)) } |]

-- | Flipping the image horizontally
imageHFlip :: TCODImage -> IO ()
imageHFlip (TCODImage i) = [C.exp| void { TCOD_image_hflip($(void* i)) } |]

-- | Flipping the image vertically
imageVFlip :: TCODImage -> IO ()
imageVFlip (TCODImage i) = [C.exp| void { TCOD_image_vflip($(void* i)) } |]

-- | Rotate the image clockwise by increment of 90 degrees.
imageRotate90 :: TCODImage
  -> Int -- ^ Number of rotations
  -> IO ()
imageRotate90 (TCODImage i) n = [C.exp| void { TCOD_image_rotate90($(void* i), $(int n')) } |]
  where n' = fromIntegral n

-- | Rotate the image clockwise by increment of 90 degrees.
imageScale :: TCODImage
  -> Int -- ^ New width
  -> Int -- ^ New height
  -> IO ()
imageScale (TCODImage i) w h = [C.exp| void { TCOD_image_scale($(void* i), $(int w'), $(int h')) } |]
  where w' = fromIntegral w
        h' = fromIntegral h

-- | You can save an image to a 24 bits .bmp or .png file.
imageSave :: TCODImage
  -> FilePath
  -> IO ()
imageSave (TCODImage i) p = withCString p $ \p' -> [C.exp| void { TCOD_image_save($(void* i), $(const char* p')) } |]

-- | You can read the size of an image in pixels with this function.
imageGetSize :: TCODImage -> IO (Int, Int)
imageGetSize (TCODImage i) = alloca $ \wp -> alloca $ \hp -> do
  [C.exp| void { TCOD_image_get_size($(void* i), $(int* wp), $(int* hp)) } |]
  let pk = fmap fromIntegral . peek
  (,) <$> pk wp <*> pk hp

-- | You can read the colors from an image with this function.
imageGetPixel :: TCODImage -> Int -> Int -> IO Color
imageGetPixel (TCODImage i) w h = alloca $ \cp -> do
  let w' = fromIntegral w
      h' = fromIntegral h
  [C.exp| void { *$(TCOD_color_t* cp) = TCOD_image_get_pixel($(void* i), $(int w'), $(int h')) } |]
  peek cp

-- | If you have set a key color for this image with setKeyColor, or if this image was created from a 32 bits
-- PNG file (with alpha layer), you can get the pixel transparency with this function. This function returns a
-- value between 0 (transparent pixel) and 255 (opaque pixel).
imageGetAlpha :: TCODImage -> Int -> Int -> IO Int
imageGetAlpha (TCODImage i) w h = do
  let w' = fromIntegral w
      h' = fromIntegral h
  fromIntegral <$> [C.exp| int { TCOD_image_get_alpha($(void* i), $(int w'), $(int h')) } |]

-- | This method uses mipmaps to get the average color of an arbitrary rectangular region of the image.
-- It can be used to draw a scaled-down version of the image. It's used by libtcod's blitting functions.
imageGetMipmapPixel :: TCODImage
  -> Float -- ^ x0
  -> Float -- ^ y0
  -> Float -- ^ x1
  -> Float -- ^ x2
  -> IO Color
imageGetMipmapPixel (TCODImage i) x0 y0 x1 y1 = alloca $ \cp -> do
  let x0' = realToFrac x0
      x1' = realToFrac x1
      y0' = realToFrac y0
      y1' = realToFrac y1
  [C.exp| void { *$(TCOD_color_t* cp) = TCOD_image_get_mipmap_pixel($(void* i), $(float x0'), $(float y0'), $(float x1'), $(float y1')) } |]
  peek cp

-- | Changing the color of a pixel
imagePutPixel :: TCODImage -> Int -> Int -> Color -> IO ()
imagePutPixel (TCODImage i) x y c = with c $ \c' -> do
  let x' = fromIntegral x
      y' = fromIntegral y
  [C.exp| void { TCOD_image_put_pixel($(void* i), $(int x'), $(int y'), *$(TCOD_color_t* c')) } |]

-- | Blitting with scaling and/or rotation
--
-- This function allows you to specify the floating point coordinates of the center
-- of the image, its scale and its rotation angle.
imageBlit :: TCODImage -- ^ the image handler, obtained with the load function.
  -> TCODConsole -- ^ The console on which the image will be drawn.
  -> Float -- ^ x Coordinates in the console of the center of the image.
  -> Float -- ^ y Coordinates in the console of the center of the image.
  -> TCODBackgroundFlag -- ^ 	This flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'.
  -> Float -- ^ scale x Scale coefficient. Must be > 0.0.
  -> Float -- ^ scale y Scale coefficient. Must be > 0.0.
  -> Float -- ^ angle Rotation angle in radians.
  -> IO ()
imageBlit (TCODImage i) (TCODConsole c) x y bf scalex scaley angle = do
  let x' = realToFrac x
      y' = realToFrac y
      bf' = fromIntegral . fromEnum $ bf
      scalex' = realToFrac scalex
      scaley' = realToFrac scaley
      angle' = realToFrac angle
  [C.exp| void { TCOD_image_blit($(void* i), $(void* c), $(float x'), $(float y'), (TCOD_bkgnd_flag_t)$(int bf'), $(float scalex'), $(float scaley'), $(float angle')) } |]

-- | This function blits a rectangular part of the image on a console without scaling it or rotating it.
-- Each pixel of the image fills a console cell.
imageBlitRect :: TCODImage -- ^ the image handler, obtained with the load function.
  -> TCODConsole -- ^ The console on which the image will be drawn.
  -> Int -- ^ x Coordinates in the console of the upper-left corner of the image.
  -> Int -- ^ y Coordinates in the console of the upper-left corner of the image.
  -> Int -- ^ w Dimension of the image on the console. Use -1,-1 to use the image size.
  -> Int -- ^ h Dimension of the image on the console. Use -1,-1 to use the image size.
  -> TCODBackgroundFlag -- ^ 	This flag defines how the cell's background color is modified. See 'TCODBackgroundFlag'.
  -> IO ()
imageBlitRect (TCODImage i) (TCODConsole c) x y w h bf = do
  let x' = fromIntegral x
      y' = fromIntegral y
      w' = fromIntegral w
      h' = fromIntegral h
      bf' = fromIntegral . fromEnum $ bf
  [C.exp| void { TCOD_image_blit_rect($(void* i), $(void* c), $(int x'), $(int y'), $(int w'), $(int h'), (TCOD_bkgnd_flag_t)$(int bf')) } |]

-- | Blitting with subcell resolution
--
-- Eventually, you can use some special characters in the libtcod fonts to double the console resolution using this blitting function.
imageBlit2X :: TCODImage -- ^ the image handler, obtained with the load function.
  -> TCODConsole -- ^ The console of which the image will be blited. Foreground, background and character data will be overwritten.
  -> Int -- ^ dx Coordinate of the console cell where the upper left corner of the blitted image will be.
  -> Int -- ^ dy Coordinate of the console cell where the upper left corner of the blitted image will be.
  -> Int -- ^ sx Part of the image to blit. Use -1 in w and h to blit the whole image.
  -> Int -- ^ sy Part of the image to blit. Use -1 in w and h to blit the whole image.
  -> Int -- ^ w Part of the image to blit. Use -1 in w and h to blit the whole image.
  -> Int -- ^ h Part of the image to blit. Use -1 in w and h to blit the whole image.
  -> IO ()
imageBlit2X (TCODImage i) (TCODConsole c) dx dy sx sy w h = do
  let dx' = fromIntegral dx
      dy' = fromIntegral dy
      sx' = fromIntegral sx
      sy' = fromIntegral sy
      w' = fromIntegral w
      h' = fromIntegral h
  [C.exp| void { TCOD_image_blit_2x($(void* i), $(void* c), $(int dx'), $(int dy'), $(int sx'), $(int sy'), $(int w'), $(int h')) } |]

-- | Free memory of the image
imageDelete :: TCODImage -> IO ()
imageDelete (TCODImage i) = [C.exp| void {TCOD_image_delete($(void* i))} |]

-- | When blitting an image, you can define a key color that will be ignored by the blitting function.
-- This makes it possible to blit non rectangular images or images with transparent pixels.
imageSetKeyColor :: TCODImage -> Color -> IO ()
imageSetKeyColor (TCODImage i) c = with c $ \c' ->
  [C.exp| void {TCOD_image_set_key_color($(void* i), *$(TCOD_color_t* c'))} |]

-- | You can use this simpler version (for images with alpha layer, returns true only if alpha == 0) :
imageIsPixelTransparent :: TCODImage -> Int -> Int -> IO Bool
imageIsPixelTransparent (TCODImage i) x y = do
  let x' = fromIntegral x
      y' = fromIntegral y
  toBool <$> [C.exp| int {(int)TCOD_image_is_pixel_transparent($(void* i), $(int x'), $(int y'))} |]
