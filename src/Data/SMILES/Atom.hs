module Data.SMILES.Atom where

import Data.Text (Text)

data Atom = BracketAtom Bracket
          | SimpleAtom AtomSymbol
  deriving (Show, Read, Eq, Ord)

data AtomSymbol = AromaticAtom Organic
                | AliphaticAtom Organic
                | OtherAtom Text
                | WildcardAtom
  deriving (Show, Read, Eq, Ord)

data Organic = F | Cl | Br | I | B | C | N | O | S | P
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

data Bracket = Bracket { bracketSymbol    :: AtomSymbol -- element | *
                       , bracketIsotope   :: Maybe Int  -- \d
                       , bracketChirality :: Maybe Chirality
                       , bracketHCount    :: Maybe Int  -- H | H\d
                       , bracketCharge    :: Maybe Int  -- + | - | +\d | -\d
                       , bracketClass     :: Maybe Int  -- :class
                       }
  deriving (Show, Read, Eq, Ord)

data Chirality = AntiClockwise | Clockwise |
                  TH1 |  TH2 |
                  AL1 |  AL2 |  SP1 |  SP2 |  SP3 |
                  TB1 |  TB2 |  TB3 |  TB4 |  TB5 |  TB6 |  TB7 |  TB8 |  TB9 | TB10 |
                 TB11 | TB12 | TB13 | TB14 | TB15 | TB16 | TB17 | TB18 | TB19 | TB20 |
                  OH1 |  OH2 |  OH3 |  OH4 |  OH5 |  OH6 |  OH7 |  OH8 |  OH9 | OH10 |
                 OH11 | OH12 | OH13 | OH14 | OH15 | OH16 | OH17 | OH18 | OH19 | OH20 |
                 OH21 | OH22 | OH23 | OH24 | OH25 | OH26 | OH27 | OH28 | OH29 | OH30
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
