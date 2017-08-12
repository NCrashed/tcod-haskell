import Data.Maybe
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)
import System.Directory

import qualified Distribution.PackageDescription as PD

main = defaultMainWithHooks simpleUserHooks {
    preConf = \a b -> makeLib a b >> preConf simpleUserHooks a b
  , confHook = myConfHook
  }

makeLib :: Args -> ConfigFlags -> IO ()
makeLib _ flags = withCurrentDirectory "libtcod/build/autotools" $ do
  rawSystemExit (fromFlag $ configVerbosity flags) "env" ["autoreconf", "-i"]
  rawSystemExit (fromFlag $ configVerbosity flags) "env" ["./configure", "CFLAGS=-O2"]
  rawSystemExit (fromFlag $ configVerbosity flags) "env" ["make"]

myConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
myConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library
  dir <- getCurrentDirectory
  pure localBuildInfo {
      localPkgDescr = packageDescription {
          PD.library = Just $ library {
              PD.libBuildInfo = libraryBuildInfo {
                  PD.extraLibDirs = (dir ++ "/libtcod/build/autotools/.libs"):PD.extraLibDirs libraryBuildInfo
                }
            }
        }
    }
