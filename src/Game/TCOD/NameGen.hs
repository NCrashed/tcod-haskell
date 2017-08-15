{-# LANGUAGE QuasiQuotes #-}
-- | This tool allows one to generate random names out of custom made syllable sets.
module Game.TCOD.NameGen(
    TCODNameGen(..)
  , namegenParse
  , namegenDestroy
  , namegenGenerate
  , namegenGenerateCustom
  , namegenGetSets
  ) where

import Foreign
import Foreign.C
import Game.TCOD.Context as C
import Game.TCOD.List
import Game.TCOD.MersenneTypes
import GHC.Generics

context tcodContext
verbatim "#define TCOD_SDL2"
include "libtcod/namegen.h"

-- | Reference to name generator state
newtype TCODNameGen = TCODNameGen { unTCODNameGen :: Ptr () }
  deriving (Eq, Ord, Show, Generic)

-- | Parse a file with syllable sets
--
-- In order to be able to generate names, the name generator needs to be fed proper data. It will then be ready to generate random names defined in the file(s) it is fed. Syllable set parsing is achieved via the following.
--
-- Note 1: Each file will be parsed once only. If, for some reason, you would like to parse the same file twice, you will need to destroy the generator first, which will empty the list of parsed files along with erasing all the data retrieved from those files.
--
-- Note 2: The generator can be fed data multiple times if you have it in separate files. Just make sure the structure names in them aren't duplicated, otherwise they will be silently ignored.
namegenParse :: FilePath -> TCODRandom -> IO ()
namegenParse fn (TCODRandom r) = withCString fn $ \fn' ->
  [C.exp| void {TCOD_namegen_parse($(const char* fn'), $(void* r))}|]

-- | Destroy name generator
namegenDestroy :: IO ()
namegenDestroy = [C.exp| void {TCOD_namegen_destroy()}|]

-- | Generating a default name
--
-- The following will output a random name generated using one of the generation rules specified in the syllable set:
-- The name you specify needs to be in one of the files the generator has previously parsed (see Creating a generator). If such a name doesn't exist, a warning will be displayed and NULL will be returned.
namegenGenerate :: String -- ^ Name. The structure name you wish to refer to, for instance, "celtic female".
  -> IO (Maybe String)
namegenGenerate n = withCString n $ \n' -> do
  ptr <- [C.exp| char* { TCOD_namegen_generate($(char* n'), false) }|]
  if ptr == nullPtr then pure Nothing else Just <$> peekCString ptr

-- | Generating a custom name
--
-- It is also possible to generate a name using custom generation rules. This
--  overrides the random choice of a generation rule from the syllable set.
-- Please refer to tcod docs chapter 16.5 to learn about the name generation rules syntax.
namegenGenerateCustom :: String -- ^ Name. The structure name you wish to refer to, for instance, "celtic female".
  -> String -- ^ Rule.
  -> IO (Maybe String)
namegenGenerateCustom n r = withCString n $ \n' -> withCString r $ \r' -> do
  ptr <- [C.exp| char* { TCOD_namegen_generate_custom($(char* n'), $(char* r'), false) }|]
  if ptr == nullPtr then pure Nothing else Just <$> peekCString ptr

-- | Retrieve the list of all available syllable set names
namegenGetSets :: IO (TCODList String)
namegenGetSets = TCODList <$> [C.exp| void* { TCOD_namegen_get_sets() }|]
