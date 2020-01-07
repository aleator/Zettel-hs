module Prelude
       ( module Relude
       , module Flow
       , (.#>), (.<#)
       ) where

import Relude
import Relude.Extra.Newtype
import Flow

f .#> g  = g #. f
f .<# g  = f #. g
