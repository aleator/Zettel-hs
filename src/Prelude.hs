module Prelude
       ( module Relude
       , module Flow
       , (.#>), (.<#)
       ) where

import Relude
import Relude.Extra.Newtype
import Flow

(.#>) :: Coercible b c => (a -> b) -> (b -> c) -> a -> c
f .#> g  = g #. f

(.<#) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
f .<# g  = f #. g
