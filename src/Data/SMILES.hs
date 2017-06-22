module Data.SMILES where

import           Data.SMILES.Atom
import           Data.SMILES.Bond

newtype SMILES = SMILES { getSMILES :: [ChainToken] }
  deriving (Show, Read, Eq, Ord)

data ChainToken = Atom Atom
                | Bond Bond
                | RingClosure (Maybe Bond) Int
                | Branch SMILES
  deriving (Show, Read, Eq, Ord)
