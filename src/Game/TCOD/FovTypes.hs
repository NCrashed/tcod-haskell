module Game.TCOD.FovTypes(
    TCODFovAlgorithm(..)
  ) where

import GHC.Generics

-- | Supported FOV algorithms
data TCODFovAlgorithm =
    FovBasic -- ^ http://roguebasin.roguelikedevelopment.org/index.php?title=Ray_casting
  | FovDiamond -- ^ http://www.geocities.com/temerra/los_rays.html
  | FovShadow -- ^ http://roguebasin.roguelikedevelopment.org/index.php?title=FOV_using_recursive_shadowcasting
  | FovPermissive0 -- ^ http://roguebasin.roguelikedevelopment.org/index.php?title=Precise_Permissive_Field_of_View
  | FovPermissive1
  | FovPermissive2
  | FovPermissive3
  | FovPermissive4
  | FovPermissive5
  | FovPermissive6
  | FovPermissive7
  | FovPermissive8
  | FovRestrictive -- ^ Mingos' Restrictive Precise Angle Shadowcasting (contribution by Mingos)
  deriving (Eq, Ord, Enum, Show, Read, Bounded, Generic)
