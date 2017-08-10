module Main where

import Game.TCOD

main :: IO ()
main = withSystem $ do
  consoleInitRoot 80 50 "libtcod Haskell example" False RendererSDL
  unlessM consoleIsWindowClosed $ do
    _ <- systemCheckForEvent [EventKeyPress]
    consoleClear rootConsole
    consolePutChar rootConsole 40 25 '@' BackgroundDefault
    consoleFlush

-- | Do action until given condition predicate doesn't return 'False'
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mcond ma = do
  cond <- mcond
  if not cond then do
    ma
    unlessM mcond ma
  else pure ()
