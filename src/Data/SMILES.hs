module Data.SMILES
  ( ChainToken (..)
  , SMILES (..)
  ) where

import           Data.SMILES.Atom (Atom (..))
import           Data.SMILES.Bond (Bond (..))

newtype SMILES = SMILES { getSMILES :: [ChainToken] }
  deriving (Show, Read, Eq, Ord)

data ChainToken = Atom Atom
                | Bond Bond
                | RingClosure (Maybe Bond) Int
                | Branch SMILES
  deriving (Show, Read, Eq, Ord)
