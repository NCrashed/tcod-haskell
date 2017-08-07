{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
module Game.TCOD.Color(
    Color(..)
  , ColorName(..)
  , ColorLevel(..)
  -- * Constructors
  , colorRGB
  , colorHSV
  -- * Basic operations
  , colorEquals
  , colorAdd
  , colorSubtract
  , colorMultiply
  , colorMultiplyScalar
  , colorLerp
  -- * HSV transformations
  , colorPtrSetHSV
  , colorSetHSV
  , colorGetHSV
  , colorGetHue
  , colorPtrSetHue
  , colorSetHue
  , colorGetSaturation
  , colorPtrSetSaturation
  , colorSetSaturation
  , colorGetValue
  , colorPtrSetValue
  , colorSetValue
  , colorPtrShiftHue
  , colorShiftHue
  , colorPtrScaleHSV
  , colorScaleHSV
  -- * Color map
  , colorGenMapRaw
  , colorGenMap
  -- * Color array
  , colors
  -- ** Grey levels
  , black
  , darkestGrey
  , darkerGrey
  , darkGrey
  , grey
  , lightGrey
  , lighterGrey
  , lightestGrey
  , darkestGray
  , darkerGray
  , darkGray
  , gray
  , lightGray
  , lighterGray
  , lightestGray
  , white
  -- ** Sepia
  , darkestSepia
  , darkerSepia
  , darkSepia
  , sepia
  , lightSepia
  , lighterSepia
  , lightestSepia
  -- ** Standard colors
  , red
  , flame
  , orange
  , amber
  , yellow
  , lime
  , chartreuse
  , green
  , sea
  , turquoise
  , cyan
  , sky
  , azure
  , blue
  , han
  , violet
  , purple
  , fuchsia
  , magenta
  , pink
  , crimson
  -- ** Dark colors
  , darkRed
  , darkFlame
  , darkOrange
  , darkAmber
  , darkYellow
  , darkLime
  , darkChartreuse
  , darkGreen
  , darkSea
  , darkTurquoise
  , darkCyan
  , darkSky
  , darkAzure
  , darkBlue
  , darkHan
  , darkViolet
  , darkPurple
  , darkFuchsia
  , darkMagenta
  , darkPink
  , darkCrimson
  -- ** Darker colors
  , darkerRed
  , darkerFlame
  , darkerOrange
  , darkerAmber
  , darkerYellow
  , darkerLime
  , darkerChartreuse
  , darkerGreen
  , darkerSea
  , darkerTurquoise
  , darkerCyan
  , darkerSky
  , darkerAzure
  , darkerBlue
  , darkerHan
  , darkerViolet
  , darkerPurple
  , darkerFuchsia
  , darkerMagenta
  , darkerPink
  , darkerCrimson
  -- ** Darkest colors
  , darkestRed
  , darkestFlame
  , darkestOrange
  , darkestAmber
  , darkestYellow
  , darkestLime
  , darkestChartreuse
  , darkestGreen
  , darkestSea
  , darkestTurquoise
  , darkestCyan
  , darkestSky
  , darkestAzure
  , darkestBlue
  , darkestHan
  , darkestViolet
  , darkestPurple
  , darkestFuchsia
  , darkestMagenta
  , darkestPink
  , darkestCrimson
  -- ** Light colors
  , lightRed
  , lightFlame
  , lightOrange
  , lightAmber
  , lightYellow
  , lightLime
  , lightChartreuse
  , lightGreen
  , lightSea
  , lightTurquoise
  , lightCyan
  , lightSky
  , lightAzure
  , lightBlue
  , lightHan
  , lightViolet
  , lightPurple
  , lightFuchsia
  , lightMagenta
  , lightPink
  , lightCrimson
  -- ** Lighter colors
  , lighterRed
  , lighterFlame
  , lighterOrange
  , lighterAmber
  , lighterYellow
  , lighterLime
  , lighterChartreuse
  , lighterGreen
  , lighterSea
  , lighterTurquoise
  , lighterCyan
  , lighterSky
  , lighterAzure
  , lighterBlue
  , lighterHan
  , lighterViolet
  , lighterPurple
  , lighterFuchsia
  , lighterMagenta
  , lighterPink
  , lighterCrimson
  -- ** Lightest colors
  , lightestRed
  , lightestFlame
  , lightestOrange
  , lightestAmber
  , lightestYellow
  , lightestLime
  , lightestChartreuse
  , lightestGreen
  , lightestSea
  , lightestTurquoise
  , lightestCyan
  , lightestSky
  , lightestAzure
  , lightestBlue
  , lightestHan
  , lightestViolet
  , lightestPurple
  , lightestFuchsia
  , lightestMagenta
  , lightestPink
  , lightestCrimson
  -- ** Desaturated
  , desaturatedRed
  , desaturatedFlame
  , desaturatedOrange
  , desaturatedAmber
  , desaturatedYellow
  , desaturatedLime
  , desaturatedChartreuse
  , desaturatedGreen
  , desaturatedSea
  , desaturatedTurquoise
  , desaturatedCyan
  , desaturatedSky
  , desaturatedAzure
  , desaturatedBlue
  , desaturatedHan
  , desaturatedViolet
  , desaturatedPurple
  , desaturatedFuchsia
  , desaturatedMagenta
  , desaturatedPink
  , desaturatedCrimson
  -- ** Metallic
  , brass
  , copper
  , gold
  , silver
  -- * Miscellaneous
  , celadon
  , peach
  ) where

import Game.TCOD.Context as C

import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Ord
import Data.Vector.Storable (Vector)
import Foreign
import Foreign.C.Types
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as V

context tcodContext
#include "color.h" -- hsc2hs
include "color.h" -- inline-c

instance Storable Color where
  sizeOf    _ = #{size TCOD_color_t}
  alignment _ = #{alignment TCOD_color_t}
  poke p Color{..} = do
    #{poke TCOD_color_t, r} p colorR
    #{poke TCOD_color_t, g} p colorG
    #{poke TCOD_color_t, b} p colorB
  peek p = Color
    <$> (#{peek TCOD_color_t, r} p)
    <*> (#{peek TCOD_color_t, g} p)
    <*> (#{peek TCOD_color_t, b} p)

-- | Color names
data ColorName =
    ColorRed
  | ColorFlame
  | ColorOrange
  | ColorAmber
  | ColorYellow
  | ColorLime
  | ColorChartreuse
  | ColorGreen
  | ColorSea
  | ColorTurquoise
  | ColorCyan
  | ColorSky
  | ColorAzure
  | ColorBlue
  | ColorHan
  | ColorViolet
  | ColorPurple
  | ColorFuchsia
  | ColorMagenta
  | ColorPink
  | ColorCrimson
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Color levels
data ColorLevel =
    ColorDesaturated
  | ColorLightest
  | ColorLighter
  | ColorLight
  | ColorNormal
  | ColorDark
  | ColorDarker
  | ColorDarkest
  | ColorLevels
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- constructors

-- | Construct RGB color from int8 on C side
colorRGB :: Word8 -> Word8 -> Word8 -> Color
colorRGB r g b = unsafePerformIO $ alloca $ \ptr -> do
  [C.exp| void { *$(TCOD_color_t* ptr) = TCOD_color_RGB($(unsigned char r'), $(unsigned char g'), $(unsigned char b')) } |]
  peek ptr
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b

-- | Construct HSV (hue-saturation-value) colors from words on C side
colorHSV :: Float -> Float -> Float -> Color
colorHSV r g b = unsafePerformIO $ alloca $ \ptr -> do
  [C.exp| void { *$(TCOD_color_t* ptr) = TCOD_color_HSV($(float r'), $(float g'), $(float b')) } |]
  peek ptr
  where r' = realToFrac r
        g' = realToFrac g
        b' = realToFrac b

-- basic operations

-- | Compare two colors using C side
colorEquals :: Color -> Color -> Bool
colorEquals c1 c2 = unsafePerformIO $ with c1 $ \c1' -> with c2 $ \c2' ->
  toBool <$> [C.exp| int { (int)TCOD_color_equals(*$(TCOD_color_t* c1'), *$(TCOD_color_t* c2')) } |]

-- | Add two colors using C side
colorAdd :: Color -> Color -> Color
colorAdd c1 c2 = unsafePerformIO $ with c1 $ \c1' -> with c2 $ \c2' -> alloca $ \ptr -> do
  [C.exp| void { *$(TCOD_color_t* ptr) = TCOD_color_add(*$(TCOD_color_t* c1'), *$(TCOD_color_t* c2')) } |]
  peek ptr

-- | Subtract two colors using C side
colorSubtract :: Color -> Color -> Color
colorSubtract c1 c2 = unsafePerformIO $ with c1 $ \c1' -> with c2 $ \c2' -> alloca $ \ptr -> do
  [C.exp| void { *$(TCOD_color_t* ptr) = TCOD_color_subtract(*$(TCOD_color_t* c1'), *$(TCOD_color_t* c2')) } |]
  peek ptr

-- | Multiply two colors using C side
colorMultiply :: Color -> Color -> Color
colorMultiply c1 c2 = unsafePerformIO $ with c1 $ \c1' -> with c2 $ \c2' -> alloca $ \ptr -> do
  [C.exp| void { *$(TCOD_color_t* ptr) = TCOD_color_multiply(*$(TCOD_color_t* c1'), *$(TCOD_color_t* c2')) } |]
  peek ptr

-- | Multiply color and scalar using C side
colorMultiplyScalar :: Color -> Float -> Color
colorMultiplyScalar c1 v = unsafePerformIO $ with c1 $ \c1' -> alloca $ \ptr -> do
  let v' = realToFrac v
  [C.exp| void { *$(TCOD_color_t* ptr) = TCOD_color_multiply_scalar(*$(TCOD_color_t* c1'), $(float v')) } |]
  peek ptr

-- | Linear interpolation between two colors
colorLerp :: Color -> Color -> Float -> Color
colorLerp c1 c2 v = unsafePerformIO $ with c1 $ \c1' -> with c2 $ \c2' -> alloca $ \ptr -> do
  let v' = realToFrac v
  [C.exp| void { *$(TCOD_color_t* ptr) = TCOD_color_lerp(*$(TCOD_color_t* c1'), *$(TCOD_color_t* c2'), $(float v')) } |]
  peek ptr

-- * HSV transformations

-- | Set hue, saturation and value
colorPtrSetHSV :: Ptr Color -> Float -> Float -> Float -> IO ()
colorPtrSetHSV c h s v = do
  let h' = realToFrac h
      s' = realToFrac s
      v' = realToFrac v
  [C.exp| void { TCOD_color_set_HSV($(TCOD_color_t* c), $(float h'), $(float s'), $(float v')) } |]

-- | Pure version of 'colorPtrSetHSV'
colorSetHSV :: Float -> Float -> Float -> Color -> Color
colorSetHSV h s v c = unsafePerformIO $ with c $ \c' -> do
  colorPtrSetHSV c' h s v
  peek c'

-- | Extract hue, saturation and value
colorGetHSV :: Color -> (Float, Float, Float)
colorGetHSV c = unsafePerformIO $ with c $ \c' -> alloca $ \hp -> alloca $ \sp -> alloca $ \vp -> do
  [C.exp| void { TCOD_color_get_HSV(*$(TCOD_color_t* c'), $(float* hp), $(float* sp), $(float* vp)) } |]
  let pk = fmap realToFrac . peek
  (,,) <$> pk hp <*> pk sp <*> pk vp

-- | Extract hue from color
colorGetHue :: Color -> Float
colorGetHue c =  unsafePerformIO $ with c $ \c' ->
  realToFrac <$> [C.exp| float { TCOD_color_get_hue(*$(TCOD_color_t* c')) } |]

-- | Set hue value for color in C-side
colorPtrSetHue :: Ptr Color -> Float -> IO ()
colorPtrSetHue c v = do
  let v' = realToFrac v
  [C.exp| void { TCOD_color_set_hue($(TCOD_color_t* c), $(float v')) } |]

-- | Pure version of 'colorPtrSetHue'
colorSetHue :: Float -> Color -> Color
colorSetHue v c = unsafePerformIO $ with c $ \c' -> do
  colorPtrSetHue c' v
  peek c'

-- | Extract saturation from color
colorGetSaturation :: Color -> Float
colorGetSaturation c =  unsafePerformIO $ with c $ \c' ->
  realToFrac <$> [C.exp| float { TCOD_color_get_saturation(*$(TCOD_color_t* c')) } |]

-- | Set saturation value for color in C-side
colorPtrSetSaturation :: Ptr Color -> Float -> IO ()
colorPtrSetSaturation c v = do
  let v' = realToFrac v
  [C.exp| void { TCOD_color_set_saturation($(TCOD_color_t* c), $(float v')) } |]

-- | Pure version of 'colorPtrSetSaturation'
colorSetSaturation :: Float -> Color -> Color
colorSetSaturation v c = unsafePerformIO $ with c $ \c' -> do
  colorPtrSetSaturation c' v
  peek c'

-- | Extract value from color
colorGetValue :: Color -> Float
colorGetValue c =  unsafePerformIO $ with c $ \c' ->
  realToFrac <$> [C.exp| float { TCOD_color_get_value(*$(TCOD_color_t* c')) } |]

-- | Set value value for color in C-side
colorPtrSetValue :: Ptr Color -> Float -> IO ()
colorPtrSetValue c v = do
  let v' = realToFrac v
  [C.exp| void { TCOD_color_set_value($(TCOD_color_t* c), $(float v')) } |]

-- | Pure version of 'colorPtrSetValue'
colorSetValue :: Float -> Color -> Color
colorSetValue v c = unsafePerformIO $ with c $ \c' -> do
  colorPtrSetValue c' v
  peek c'

-- | Shift hue for color in C-side
colorPtrShiftHue :: Ptr Color -> Float -> IO ()
colorPtrShiftHue c v = do
  let v' = realToFrac v
  [C.exp| void { TCOD_color_shift_hue($(TCOD_color_t* c), $(float v')) } |]

-- | Pure version of 'colorPtrShiftHue'
colorShiftHue :: Float -> Color -> Color
colorShiftHue v c = unsafePerformIO $ with c $ \c' -> do
  colorPtrShiftHue c' v
  peek c'

-- | Scale HSV for color in C-side
colorPtrScaleHSV :: Ptr Color -> Float -> Float -> IO ()
colorPtrScaleHSV c scoef vcoef = do
  let scoef' = realToFrac scoef
      vcoef' = realToFrac vcoef
  [C.exp| void { TCOD_color_scale_HSV($(TCOD_color_t* c), $(float scoef'), $(float vcoef')) } |]

-- | Pure version of 'colorPtrScaleHSV'
colorScaleHSV :: Float -> Float -> Color -> Color
colorScaleHSV scoef vcoef c = unsafePerformIO $ with c $ \c' -> do
  colorPtrScaleHSV c' scoef vcoef
  peek c'

-- | Raw binding for `TCOD_color_gen_map`
colorGenMapRaw :: Ptr Color -- ^ map
  -> CInt -- ^ nbKey
  -> Ptr Color -- ^ keyColor array
  -> Ptr CInt -- ^ key index array
  -> IO ()
colorGenMapRaw mp nbKey keyColor keyIndex = do
  [C.exp| void {TCOD_color_gen_map($(TCOD_color_t* mp), $(int nbKey), $(TCOD_color_t* keyColor), $(int* keyIndex))} |]

-- | High-level binding that allows to generate smooth color range from given key colors.
colorGenMap :: Map Color Int -> Vector Color
colorGenMap colorIndex
  | M.null colorIndex = mempty
  | otherwise = unsafePerformIO $ do
    let (keyColors, keyIndecies) = unzip . sortBy (comparing snd) . M.toList $ colorIndex
        n = length colorIndex :: Int
        keyColorsV = V.fromListN n keyColors :: Vector Color
        keyIndeciesV = V.fromListN n . fmap fromIntegral $ keyIndecies :: Vector CInt
        m = fromIntegral $ V.last keyIndeciesV + 1 :: Int
    mapVecPtr <- mallocForeignPtrArray m :: IO (ForeignPtr Color)
    V.unsafeWith keyColorsV $ \keyColorsV' -> V.unsafeWith keyIndeciesV $ \keyIndeciesV' -> withForeignPtr mapVecPtr $ \mapVecPtr' ->
      colorGenMapRaw mapVecPtr' (fromIntegral m) keyColorsV' keyIndeciesV'
    pure $ V.unsafeFromForeignPtr0 mapVecPtr m

-- | Get color by name and level
colors :: ColorName -> ColorLevel -> Color
colors cn cl = unsafePerformIO $ alloca $ \c -> do
  let cn' = fromIntegral . fromEnum $ cn
      cl' = fromIntegral . fromEnum $ cl
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_colors[$(int cn')][$(int cl')]} |]
  peek c

black :: Color
black = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_black} |]
  peek c

darkestGrey :: Color
darkestGrey = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_darkest_grey} |]
  peek c

darkerGrey :: Color
darkerGrey = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_darker_grey} |]
  peek c

darkGrey :: Color
darkGrey = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_dark_grey} |]
  peek c

grey :: Color
grey = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_grey} |]
  peek c

lightGrey :: Color
lightGrey = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_light_grey} |]
  peek c

lighterGrey :: Color
lighterGrey = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_lighter_grey} |]
  peek c

lightestGrey :: Color
lightestGrey = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_lightest_grey} |]
  peek c

darkestGray :: Color
darkestGray = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_darkest_gray} |]
  peek c

darkerGray :: Color
darkerGray = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_darker_gray} |]
  peek c

darkGray :: Color
darkGray = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_dark_gray} |]
  peek c

gray :: Color
gray = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_gray} |]
  peek c

lightGray :: Color
lightGray = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_light_gray} |]
  peek c

lighterGray :: Color
lighterGray = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_lighter_gray} |]
  peek c

lightestGray :: Color
lightestGray = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_lightest_gray} |]
  peek c

white :: Color
white = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void {*$(TCOD_color_t* c) = TCOD_white} |]
  peek c


-- Sepia

darkestSepia :: Color
darkestSepia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_sepia} |]
  peek c

darkerSepia :: Color
darkerSepia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_sepia} |]
  peek c

darkSepia :: Color
darkSepia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_sepia} |]
  peek c

sepia :: Color
sepia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_sepia} |]
  peek c

lightSepia :: Color
lightSepia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_sepia} |]
  peek c

lighterSepia :: Color
lighterSepia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_sepia} |]
  peek c

lightestSepia :: Color
lightestSepia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_sepia} |]
  peek c


-- Standard colors

red :: Color
red = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_red} |]
  peek c

flame :: Color
flame = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_flame} |]
  peek c

orange :: Color
orange = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_orange} |]
  peek c

amber :: Color
amber = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_amber} |]
  peek c

yellow :: Color
yellow = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_yellow} |]
  peek c

lime :: Color
lime = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lime} |]
  peek c

chartreuse :: Color
chartreuse = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_chartreuse} |]
  peek c

green :: Color
green = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_green} |]
  peek c

sea :: Color
sea = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_sea} |]
  peek c

turquoise :: Color
turquoise = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_turquoise} |]
  peek c

cyan :: Color
cyan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_cyan} |]
  peek c

sky :: Color
sky = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_sky} |]
  peek c

azure :: Color
azure = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_azure} |]
  peek c

blue :: Color
blue = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_blue} |]
  peek c

han :: Color
han = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_han} |]
  peek c

violet :: Color
violet = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_violet} |]
  peek c

purple :: Color
purple = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_purple} |]
  peek c

fuchsia :: Color
fuchsia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_fuchsia} |]
  peek c

magenta :: Color
magenta = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_magenta} |]
  peek c

pink :: Color
pink = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_pink} |]
  peek c

crimson :: Color
crimson = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_crimson} |]
  peek c


-- Dark colors

darkRed :: Color
darkRed = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_red} |]
  peek c

darkFlame :: Color
darkFlame = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_flame} |]
  peek c

darkOrange :: Color
darkOrange = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_orange} |]
  peek c

darkAmber :: Color
darkAmber = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_amber} |]
  peek c

darkYellow :: Color
darkYellow = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_yellow} |]
  peek c

darkLime :: Color
darkLime = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_lime} |]
  peek c

darkChartreuse :: Color
darkChartreuse = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_chartreuse} |]
  peek c

darkGreen :: Color
darkGreen = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_green} |]
  peek c

darkSea :: Color
darkSea = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_sea} |]
  peek c

darkTurquoise :: Color
darkTurquoise = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_turquoise} |]
  peek c

darkCyan :: Color
darkCyan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_cyan} |]
  peek c

darkSky :: Color
darkSky = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_sky} |]
  peek c

darkAzure :: Color
darkAzure = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_azure} |]
  peek c

darkBlue :: Color
darkBlue = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_blue} |]
  peek c

darkHan :: Color
darkHan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_han} |]
  peek c

darkViolet :: Color
darkViolet = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_violet} |]
  peek c

darkPurple :: Color
darkPurple = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_purple} |]
  peek c

darkFuchsia :: Color
darkFuchsia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_fuchsia} |]
  peek c

darkMagenta :: Color
darkMagenta = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_magenta} |]
  peek c

darkPink :: Color
darkPink = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_pink} |]
  peek c

darkCrimson :: Color
darkCrimson = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_dark_crimson} |]
  peek c


-- Darker colors

darkerRed :: Color
darkerRed = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_red} |]
  peek c

darkerFlame :: Color
darkerFlame = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_flame} |]
  peek c

darkerOrange :: Color
darkerOrange = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_orange} |]
  peek c

darkerAmber :: Color
darkerAmber = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_amber} |]
  peek c

darkerYellow :: Color
darkerYellow = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_yellow} |]
  peek c

darkerLime :: Color
darkerLime = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_lime} |]
  peek c

darkerChartreuse :: Color
darkerChartreuse = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_chartreuse} |]
  peek c

darkerGreen :: Color
darkerGreen = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_green} |]
  peek c

darkerSea :: Color
darkerSea = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_sea} |]
  peek c

darkerTurquoise :: Color
darkerTurquoise = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_turquoise} |]
  peek c

darkerCyan :: Color
darkerCyan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_cyan} |]
  peek c

darkerSky :: Color
darkerSky = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_sky} |]
  peek c

darkerAzure :: Color
darkerAzure = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_azure} |]
  peek c

darkerBlue :: Color
darkerBlue = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_blue} |]
  peek c

darkerHan :: Color
darkerHan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_han} |]
  peek c

darkerViolet :: Color
darkerViolet = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_violet} |]
  peek c

darkerPurple :: Color
darkerPurple = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_purple} |]
  peek c

darkerFuchsia :: Color
darkerFuchsia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_fuchsia} |]
  peek c

darkerMagenta :: Color
darkerMagenta = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_magenta} |]
  peek c

darkerPink :: Color
darkerPink = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_pink} |]
  peek c

darkerCrimson :: Color
darkerCrimson = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darker_crimson} |]
  peek c


-- Darkest colors

darkestRed :: Color
darkestRed = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_red} |]
  peek c

darkestFlame :: Color
darkestFlame = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_flame} |]
  peek c

darkestOrange :: Color
darkestOrange = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_orange} |]
  peek c

darkestAmber :: Color
darkestAmber = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_amber} |]
  peek c

darkestYellow :: Color
darkestYellow = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_yellow} |]
  peek c

darkestLime :: Color
darkestLime = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_lime} |]
  peek c

darkestChartreuse :: Color
darkestChartreuse = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_chartreuse} |]
  peek c

darkestGreen :: Color
darkestGreen = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_green} |]
  peek c

darkestSea :: Color
darkestSea = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_sea} |]
  peek c

darkestTurquoise :: Color
darkestTurquoise = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_turquoise} |]
  peek c

darkestCyan :: Color
darkestCyan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_cyan} |]
  peek c

darkestSky :: Color
darkestSky = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_sky} |]
  peek c

darkestAzure :: Color
darkestAzure = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_azure} |]
  peek c

darkestBlue :: Color
darkestBlue = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_blue} |]
  peek c

darkestHan :: Color
darkestHan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_han} |]
  peek c

darkestViolet :: Color
darkestViolet = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_violet} |]
  peek c

darkestPurple :: Color
darkestPurple = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_purple} |]
  peek c

darkestFuchsia :: Color
darkestFuchsia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_fuchsia} |]
  peek c

darkestMagenta :: Color
darkestMagenta = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_magenta} |]
  peek c

darkestPink :: Color
darkestPink = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_pink} |]
  peek c

darkestCrimson :: Color
darkestCrimson = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_darkest_crimson} |]
  peek c


-- Light colors

lightRed :: Color
lightRed = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_red} |]
  peek c

lightFlame :: Color
lightFlame = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_flame} |]
  peek c

lightOrange :: Color
lightOrange = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_orange} |]
  peek c

lightAmber :: Color
lightAmber = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_amber} |]
  peek c

lightYellow :: Color
lightYellow = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_yellow} |]
  peek c

lightLime :: Color
lightLime = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_lime} |]
  peek c

lightChartreuse :: Color
lightChartreuse = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_chartreuse} |]
  peek c

lightGreen :: Color
lightGreen = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_green} |]
  peek c

lightSea :: Color
lightSea = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_sea} |]
  peek c

lightTurquoise :: Color
lightTurquoise = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_turquoise} |]
  peek c

lightCyan :: Color
lightCyan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_cyan} |]
  peek c

lightSky :: Color
lightSky = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_sky} |]
  peek c

lightAzure :: Color
lightAzure = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_azure} |]
  peek c

lightBlue :: Color
lightBlue = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_blue} |]
  peek c

lightHan :: Color
lightHan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_han} |]
  peek c

lightViolet :: Color
lightViolet = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_violet} |]
  peek c

lightPurple :: Color
lightPurple = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_purple} |]
  peek c

lightFuchsia :: Color
lightFuchsia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_fuchsia} |]
  peek c

lightMagenta :: Color
lightMagenta = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_magenta} |]
  peek c

lightPink :: Color
lightPink = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_pink} |]
  peek c

lightCrimson :: Color
lightCrimson = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_light_crimson} |]
  peek c


-- Lighter colors

lighterRed :: Color
lighterRed = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_red} |]
  peek c

lighterFlame :: Color
lighterFlame = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_flame} |]
  peek c

lighterOrange :: Color
lighterOrange = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_orange} |]
  peek c

lighterAmber :: Color
lighterAmber = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_amber} |]
  peek c

lighterYellow :: Color
lighterYellow = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_yellow} |]
  peek c

lighterLime :: Color
lighterLime = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_lime} |]
  peek c

lighterChartreuse :: Color
lighterChartreuse = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_chartreuse} |]
  peek c

lighterGreen :: Color
lighterGreen = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_green} |]
  peek c

lighterSea :: Color
lighterSea = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_sea} |]
  peek c

lighterTurquoise :: Color
lighterTurquoise = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_turquoise} |]
  peek c

lighterCyan :: Color
lighterCyan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_cyan} |]
  peek c

lighterSky :: Color
lighterSky = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_sky} |]
  peek c

lighterAzure :: Color
lighterAzure = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_azure} |]
  peek c

lighterBlue :: Color
lighterBlue = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_blue} |]
  peek c

lighterHan :: Color
lighterHan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_han} |]
  peek c

lighterViolet :: Color
lighterViolet = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_violet} |]
  peek c

lighterPurple :: Color
lighterPurple = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_purple} |]
  peek c

lighterFuchsia :: Color
lighterFuchsia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_fuchsia} |]
  peek c

lighterMagenta :: Color
lighterMagenta = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_magenta} |]
  peek c

lighterPink :: Color
lighterPink = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_pink} |]
  peek c

lighterCrimson :: Color
lighterCrimson = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lighter_crimson} |]
  peek c


-- Lightest colors

lightestRed :: Color
lightestRed = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_red} |]
  peek c

lightestFlame :: Color
lightestFlame = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_flame} |]
  peek c

lightestOrange :: Color
lightestOrange = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_orange} |]
  peek c

lightestAmber :: Color
lightestAmber = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_amber} |]
  peek c

lightestYellow :: Color
lightestYellow = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_yellow} |]
  peek c

lightestLime :: Color
lightestLime = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_lime} |]
  peek c

lightestChartreuse :: Color
lightestChartreuse = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_chartreuse} |]
  peek c

lightestGreen :: Color
lightestGreen = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_green} |]
  peek c

lightestSea :: Color
lightestSea = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_sea} |]
  peek c

lightestTurquoise :: Color
lightestTurquoise = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_turquoise} |]
  peek c

lightestCyan :: Color
lightestCyan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_cyan} |]
  peek c

lightestSky :: Color
lightestSky = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_sky} |]
  peek c

lightestAzure :: Color
lightestAzure = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_azure} |]
  peek c

lightestBlue :: Color
lightestBlue = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_blue} |]
  peek c

lightestHan :: Color
lightestHan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_han} |]
  peek c

lightestViolet :: Color
lightestViolet = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_violet} |]
  peek c

lightestPurple :: Color
lightestPurple = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_purple} |]
  peek c

lightestFuchsia :: Color
lightestFuchsia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_fuchsia} |]
  peek c

lightestMagenta :: Color
lightestMagenta = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_magenta} |]
  peek c

lightestPink :: Color
lightestPink = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_pink} |]
  peek c

lightestCrimson :: Color
lightestCrimson = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_lightest_crimson} |]
  peek c


-- Desaturated

desaturatedRed :: Color
desaturatedRed = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_red} |]
  peek c

desaturatedFlame :: Color
desaturatedFlame = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_flame} |]
  peek c

desaturatedOrange :: Color
desaturatedOrange = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_orange} |]
  peek c

desaturatedAmber :: Color
desaturatedAmber = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_amber} |]
  peek c

desaturatedYellow :: Color
desaturatedYellow = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_yellow} |]
  peek c

desaturatedLime :: Color
desaturatedLime = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_lime} |]
  peek c

desaturatedChartreuse :: Color
desaturatedChartreuse = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_chartreuse} |]
  peek c

desaturatedGreen :: Color
desaturatedGreen = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_green} |]
  peek c

desaturatedSea :: Color
desaturatedSea = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_sea} |]
  peek c

desaturatedTurquoise :: Color
desaturatedTurquoise = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_turquoise} |]
  peek c

desaturatedCyan :: Color
desaturatedCyan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_cyan} |]
  peek c

desaturatedSky :: Color
desaturatedSky = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_sky} |]
  peek c

desaturatedAzure :: Color
desaturatedAzure = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_azure} |]
  peek c

desaturatedBlue :: Color
desaturatedBlue = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_blue} |]
  peek c

desaturatedHan :: Color
desaturatedHan = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_han} |]
  peek c

desaturatedViolet :: Color
desaturatedViolet = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_violet} |]
  peek c

desaturatedPurple :: Color
desaturatedPurple = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_purple} |]
  peek c

desaturatedFuchsia :: Color
desaturatedFuchsia = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_fuchsia} |]
  peek c

desaturatedMagenta :: Color
desaturatedMagenta = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_magenta} |]
  peek c

desaturatedPink :: Color
desaturatedPink = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_pink} |]
  peek c

desaturatedCrimson :: Color
desaturatedCrimson = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_desaturated_crimson} |]
  peek c


-- Metallic

brass :: Color
brass = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_brass} |]
  peek c

copper :: Color
copper = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_copper} |]
  peek c

gold :: Color
gold = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_gold} |]
  peek c

silver :: Color
silver = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_silver} |]
  peek c


-- Miscellaneous

celadon :: Color
celadon = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_celadon} |]
  peek c

peach :: Color
peach = unsafePerformIO $ alloca $ \c -> do
  [C.exp| void { *$(TCOD_color_t* c) = TCOD_peach} |]
  peek c
