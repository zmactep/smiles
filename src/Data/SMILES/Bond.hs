module Data.SMILES.Bond where

data Bond = AliphaticBond AliphaticBond
          | AromaticBond
          | DoubleConfiguration DoubleConfiguration
  deriving (Show, Read, Eq, Ord)

data DoubleConfiguration = BelowBond | AboveBond
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
  
data AliphaticBond = SingleBond | DoubleBond | TripleBond | QuadrupleBond
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
