{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
module Game.TCOD.ConsoleTypes(
    TCODConsole(..)
  , TCODKeyCode(..)
  , TCODKey(..)
  , tcodKeyTextSize
  , TCODChar(..)
  , TCODColorControl(..)
  , TCODBackgroundFlag(..)
  , TCODKeyStatus(..)
  , TCODFontFlag(..)
  , combineFontFlags
  , TCODRenderer(..)
  , TCODAlignment(..)
  ) where

import Data.Bits
import Data.Char
import Data.Maybe
import Foreign
import Foreign.C
import GHC.Generics

import Game.TCOD.Context as C

import qualified Data.Foldable as F

context tcodContext
#include "console_types.h"
include "console_types.h"
include "string.h"

-- | Tag to track pointer to `TCOD_console_t`
newtype TCODConsole = TCODConsole { unTCODConsole :: Ptr () }
  deriving (Eq, Ord, Show, Generic)

-- | Names for keyboard keys
data TCODKeyCode =
    KeyNone
  | KeyEscape
  | KeyBackspace
  | KeyTab
  | KeyEnter
  | KeyShift
  | KeyControl
  | KeyAlt
  | KeyPause
  | KeyCapslock
  | KeyPageUp
  | KeyPageDown
  | KeyEnd
  | KeyHome
  | KeyUp
  | KeyLeft
  | KeyRight
  | KeyDown
  | KeyPrintScreen
  | KeyInsert
  | KeyDelete
  | KeyLWin
  | KeyRWin
  | KeyApps
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeyKp0
  | KeyKp1
  | KeyKp2
  | KeyKp3
  | KeyKp4
  | KeyKp5
  | KeyKp6
  | KeyKp7
  | KeyKp8
  | KeyKp9
  | KeyKpAdd
  | KeyKpSub
  | KeyKpDiv
  | KeyKpMul
  | KeyKpDec
  | KeyKpEnter
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyNumLock
  | KeyScrollLock
  | KeySpace
  | KeyChar
  | KeyText
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

tcodKeyTextSize :: Int
tcodKeyTextSize = 32

-- | Key data: special code or character or text
data TCODKey = TCODKey {
  keyCode    :: !TCODKeyCode
, keyChar    :: !(Maybe Char) -- Just if keyCode == KeyChar
, keyText    :: !(Maybe String) -- Just if keyCode == KeyText
, keyPressed :: !Bool -- ^ does this correspond to a key press or key release event ?
, keyLAlt    :: !Bool
, keyLCtrl   :: !Bool
, keyLMeta   :: !Bool
, keyRAlt    :: !Bool
, keyRCtrl   :: !Bool
, keyRMeta   :: !Bool
, keyShift   :: !Bool
} deriving (Eq, Show, Generic)

instance Storable TCODKey where
  sizeOf _ = #{size TCOD_key_t}
  alignment _ = #{alignment TCOD_key_t}
  poke p TCODKey{..} = withCString (take (tcodKeyTextSize-1) $ fromMaybe "" keyText) $ \text' -> do
    #{poke TCOD_key_t, vk} p ((fromIntegral :: Int -> CInt) . fromEnum $ keyCode)
    #{poke TCOD_key_t, c} p ((fromIntegral :: Int -> CChar) . fromMaybe 0 . fmap ord $ keyChar)
    [C.exp| void { strcpy(&(TCOD_key_t*)$(void* p')->text, $(const char* text')) } |]
    #{poke TCOD_key_t, pressed} p keyPressed
    #{poke TCOD_key_t, lalt} p keyLAlt
    #{poke TCOD_key_t, lctrl} p keyLCtrl
    #{poke TCOD_key_t, lmeta} p keyLMeta
    #{poke TCOD_key_t, ralt} p keyRAlt
    #{poke TCOD_key_t, rctrl} p keyRCtrl
    #{poke TCOD_key_t, rmeta} p keyRMeta
    #{poke TCOD_key_t, shift} p keyShift
    where p' = castPtr p
  peek p = TCODKey
    <$> (toEnum . (fromIntegral :: CInt -> Int) <$> #{peek TCOD_key_t, vk} p)
    <*> ((\c -> if c == 0 then Nothing else Just $ chr c) . (fromIntegral :: CChar -> Int) <$> #{peek TCOD_key_t, c} p)
    <*> (peekTextField =<< #{peek TCOD_key_t, text} p)
    <*> (#{peek TCOD_key_t, pressed} p)
    <*> (#{peek TCOD_key_t, lalt} p)
    <*> (#{peek TCOD_key_t, lctrl} p)
    <*> (#{peek TCOD_key_t, lmeta} p)
    <*> (#{peek TCOD_key_t, ralt} p)
    <*> (#{peek TCOD_key_t, rctrl} p)
    <*> (#{peek TCOD_key_t, rmeta} p)
    <*> (#{peek TCOD_key_t, shift} p)
    where
      peekTextField vptr = do
        str <- peekCString vptr
        pure $ if null str then Nothing else Just str

-- | Special characters
data TCODChar =
  -- single walls
    CharHline
  | CharVline
  | CharNe
  | CharNw
  | CharSe
  | CharSw
  | CharTeew
  | CharTeee
  | CharTeen
  | CharTees
  | CharCross
  --  double walls
  | CharDhline
  | CharDvline
  | CharDne
  | CharDnw
  | CharDse
  | CharDsw
  | CharDteew
  | CharDteee
  | CharDteen
  | CharDtees
  | CharDcross
  -- blocks
  | CharBlock1
  | CharBlock2
  | CharBlock3
  -- arrows
  | CharArrowN
  | CharArrowS
  | CharArrowE
  | CharArrowW
  -- arrows without tail
  | CharArrow2N
  | CharArrow2S
  | CharArrow2E
  | CharArrow2W
  -- double arrows
  | CharDarrowH
  | CharDarrowV
  -- GUI stuff
  | CharCheckboxUnset
  | CharCheckboxSet
  | CharRadioUnset
  | CharRadioSet
  -- sub-pixel resolution kit
  | CharSubpNw
  | CharSubpNe
  | CharSubpN
  | CharSubpSe
  | CharSubpDiag
  | CharSubpE
  | CharSubpSw
  -- miscellaneous
  | CharSmilie
  | CharSmilieInv
  | CharHeart
  | CharDiamond
  | CharClub
  | CharSpade
  | CharBullet
  | CharBulletInv
  | CharMale
  | CharFemale
  | CharNote
  | CharNoteDouble
  | CharLight
  | CharExclamDouble
  | CharPilcrow
  | CharSection
  | CharPound
  | CharMultiplication
  | CharFunction
  | CharReserved
  | CharHalf
  | CharOneQuarter
  | CharCopyright
  | CharCent
  | CharYen
  | CharCurrency
  | CharThreeQuarters
  | CharDivision
  | CharGrade
  | CharUmlaut
  | CharPow1
  | CharPow3
  | CharPow2
  | CharBulletSquare
  deriving (Eq, Ord, Show, Read, Generic)

instance Enum TCODChar where
  toEnum n = case n of
    196 -> CharHline
    179 -> CharVline
    191 -> CharNe
    218 -> CharNw
    217 -> CharSe
    192 -> CharSw
    180 -> CharTeew
    195 -> CharTeee
    193 -> CharTeen
    194 -> CharTees
    197 -> CharCross
    205 -> CharDhline
    186 -> CharDvline
    187 -> CharDne
    201 -> CharDnw
    188 -> CharDse
    200 -> CharDsw
    185 -> CharDteew
    204 -> CharDteee
    202 -> CharDteen
    203 -> CharDtees
    206 -> CharDcross
    176 -> CharBlock1
    177 -> CharBlock2
    178 -> CharBlock3
    24 -> CharArrowN
    25 -> CharArrowS
    26 -> CharArrowE
    27 -> CharArrowW
    30 -> CharArrow2N
    31 -> CharArrow2S
    16 -> CharArrow2E
    17 -> CharArrow2W
    29 -> CharDarrowH
    18 -> CharDarrowV
    224 -> CharCheckboxUnset
    225 -> CharCheckboxSet
    9 -> CharRadioUnset
    10 -> CharRadioSet
    226 -> CharSubpNw
    227 -> CharSubpNe
    228 -> CharSubpN
    229 -> CharSubpSe
    230 -> CharSubpDiag
    231 -> CharSubpE
    232 -> CharSubpSw
    1 -> CharSmilie
    2 -> CharSmilieInv
    3 -> CharHeart
    4 -> CharDiamond
    5 -> CharClub
    6 -> CharSpade
    7 -> CharBullet
    8 -> CharBulletInv
    11 -> CharMale
    12 -> CharFemale
    13 -> CharNote
    14 -> CharNoteDouble
    15 -> CharLight
    19 -> CharExclamDouble
    20 -> CharPilcrow
    21 -> CharSection
    156 -> CharPound
    158 -> CharMultiplication
    159 -> CharFunction
    169 -> CharReserved
    171 -> CharHalf
    172 -> CharOneQuarter
    184 -> CharCopyright
    189 -> CharCent
    190 -> CharYen
    207 -> CharCurrency
    243 -> CharThreeQuarters
    246 -> CharDivision
    248 -> CharGrade
    249 -> CharUmlaut
    251 -> CharPow1
    252 -> CharPow3
    253 -> CharPow2
    254 -> CharBulletSquare
    _   -> CharSmilie -- default case

  fromEnum v = case v of
    CharHline -> 196
    CharVline -> 179
    CharNe -> 191
    CharNw -> 218
    CharSe -> 217
    CharSw -> 192
    CharTeew -> 180
    CharTeee -> 195
    CharTeen -> 193
    CharTees -> 194
    CharCross -> 197
    CharDhline -> 205
    CharDvline -> 186
    CharDne -> 187
    CharDnw -> 201
    CharDse -> 188
    CharDsw -> 200
    CharDteew -> 185
    CharDteee -> 204
    CharDteen -> 202
    CharDtees -> 203
    CharDcross -> 206
    CharBlock1 -> 176
    CharBlock2 -> 177
    CharBlock3 -> 178
    CharArrowN -> 24
    CharArrowS -> 25
    CharArrowE -> 26
    CharArrowW -> 27
    CharArrow2N -> 30
    CharArrow2S -> 31
    CharArrow2E -> 16
    CharArrow2W -> 17
    CharDarrowH -> 29
    CharDarrowV -> 18
    CharCheckboxUnset -> 224
    CharCheckboxSet -> 225
    CharRadioUnset -> 9
    CharRadioSet -> 10
    CharSubpNw -> 226
    CharSubpNe -> 227
    CharSubpN -> 228
    CharSubpSe -> 229
    CharSubpDiag -> 230
    CharSubpE -> 231
    CharSubpSw -> 232
    CharSmilie -> 1
    CharSmilieInv -> 2
    CharHeart -> 3
    CharDiamond -> 4
    CharClub -> 5
    CharSpade -> 6
    CharBullet -> 7
    CharBulletInv -> 8
    CharMale -> 11
    CharFemale -> 12
    CharNote -> 13
    CharNoteDouble -> 14
    CharLight -> 15
    CharExclamDouble -> 19
    CharPilcrow -> 20
    CharSection -> 21
    CharPound -> 156
    CharMultiplication -> 158
    CharFunction -> 159
    CharReserved -> 169
    CharHalf -> 171
    CharOneQuarter -> 172
    CharCopyright -> 184
    CharCent -> 189
    CharYen -> 190
    CharCurrency -> 207
    CharThreeQuarters -> 243
    CharDivision -> 246
    CharGrade -> 248
    CharUmlaut -> 249
    CharPow1 -> 251
    CharPow3 -> 252
    CharPow2 -> 253
    CharBulletSquare -> 254

-- | Color control flags for console
data TCODColorControl =
    Ctrl_1
  | Ctrl_2
  | Ctrl_3
  | Ctrl_4
  | Ctrl_5
  | CtrlForeRgb
  | CtrlBackRgb
  | CtrlStop
  deriving (Eq, Ord, Show, Read, Generic)

instance Enum TCODColorControl where
  toEnum i = case i of
    1 -> Ctrl_1
    2 -> Ctrl_2
    3 -> Ctrl_3
    4 -> Ctrl_4
    5 -> Ctrl_5
    6 -> CtrlForeRgb
    7 -> CtrlBackRgb
    8 -> CtrlStop
    _ -> CtrlStop

  fromEnum v = case v of
    Ctrl_1 -> 1
    Ctrl_2 -> 2
    Ctrl_3 -> 3
    Ctrl_4 -> 4
    Ctrl_5 -> 5
    CtrlForeRgb -> 6
    CtrlBackRgb -> 7
    CtrlStop -> 8

-- | Background flag
data TCODBackgroundFlag =
    BackgroundNone
  | BackgroundSet
  | BackgroundMultiply
  | BackgroundLighten
  | BackgroundDarken
  | BackgroundScreen
  | BackgroundColorDodge
  | BackgroundColorBurn
  | BackgroundAdd
  | BackgroundAdda
  | BackgroundBurn
  | BackgroundOverlay
  | BackgroundAlph
  | BackgroundDefault
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Key status
data TCODKeyStatus = KeyPressed | KeyReleased
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum TCODKeyStatus where
  toEnum i = case i of
    1 -> KeyPressed
    2 -> KeyReleased
    _ -> KeyReleased
  fromEnum v = case v of
    KeyPressed -> 1
    KeyReleased -> 2

-- | Custom font flag
data TCODFontFlag =
    FontLayoutAsciiInCol
  | FontLayoutAsciiInRow
  | FontTypeGreyScale
  | FontLayoutTcod
  deriving (Eq, Ord, Show, Read, Bounded, Generic)

instance Enum TCODFontFlag where
  toEnum i = case i of
    1 -> FontLayoutAsciiInCol
    2 -> FontLayoutAsciiInRow
    4 -> FontTypeGreyScale
    8 -> FontLayoutTcod
  fromEnum v = case v of
    FontLayoutAsciiInCol -> 1
    FontLayoutAsciiInRow -> 2
    FontTypeGreyScale -> 4
    FontLayoutTcod -> 8

-- | Assemble flags into int field
combineFontFlags :: Foldable f => f TCODFontFlag -> Int
combineFontFlags = F.foldl' (\acc v -> acc .&. fromEnum v) 0

-- | Availiable renderers
data TCODRenderer =
    RendererGLSL
  | RendererOpenGL
  | RendererSDL
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Different text/element alignments
data TCODAlignment =
    AlignLeft
  | AlignRight
  | AlignCenter
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
