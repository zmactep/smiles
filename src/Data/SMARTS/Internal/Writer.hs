module Data.SMARTS.Internal.Writer where

import           Data.SMARTS.Internal.Types (SMARTS (..))

writeSmarts :: SMARTS -> String
writeSmarts = show
