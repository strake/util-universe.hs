module Util.Universe where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Functor.Classes
import Data.Functor.Compose
import qualified Data.List as List
import Data.Maybe
import Data.Universe.Class
import Data.Universe.Instances.Base ()
import Numeric.Natural

newtype Fn a b = Fn { unFn :: a -> b }
  deriving (Semigroup, Monoid, Universe, Finite, Functor, Applicative, Monad,
            Category, Arrow, ArrowApply, ArrowChoice, ArrowLoop)
instance (Universe a, Eq b) => Eq (Fn a b) where (==) = eq1
instance Universe a => Foldable (Fn a) where
    foldMap f (Fn φ) = foldMap (f . φ) universe
instance (Eq a, Finite a) => Traversable (Fn a) where
    sequenceA (Fn φ) =
        Fn . (\ bs a -> fromJust $ List.lookup a bs) . getCompose <$>
        traverse φ (Compose $ join (,) <$> universe)
instance Universe a => Eq1 (Fn a) where
    liftEq eq (Fn φ) (Fn χ) = liftEq eq (φ <$> universe) (χ <$> universe)

universalIndex :: (Eq a, Universe a) => a -> Natural
universalIndex a = List.genericLength $ List.takeWhile (/= a) universe
