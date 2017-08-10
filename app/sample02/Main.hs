module Main where

import Game.TCOD
import Data.Functor (void)

main :: IO ()
main = void . withSystem $ do
  consoleInitRoot 80 50 "libtcod Haskell example" False RendererSDL
  unlessM consoleIsWindowClosed (40, 25) $ \(px, py) -> do
    ei <- systemCheckForEvent [EventKeyPress]
    let (px', py') = case keyCode . tcodKey $ ei of
          KeyUp -> (px, py-1)
          KeyDown -> (px, py+1)
          KeyLeft -> (px-1, py)
          KeyRight -> (px+1, py)
          _ -> (px, py)
    consoleClear rootConsole
    consolePutChar rootConsole px' py' '@' BackgroundDefault
    consoleFlush
    pure (px', py')

-- | Do action until given condition predicate doesn't return 'False'
unlessM :: Monad m => m Bool -> a -> (a -> m a) -> m a
unlessM mcond a ma = do
  cond <- mcond
  if not cond then do
    a' <- ma a
    unlessM mcond a' ma
  else pure a
