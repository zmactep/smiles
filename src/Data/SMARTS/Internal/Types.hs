module Data.SMARTS.Internal.Types where

import           Data.List  (intercalate)
import           Data.Text  (Text, unpack)

data PrimitiveAtom = Any | AnyAliphatic | AnyAromatic | Atom Text
  deriving (Eq, Ord)

instance Show PrimitiveAtom where
  show Any          = "*"
  show AnyAliphatic = "A"
  show AnyAromatic  = "a"
  show (Atom t)     = unpack t

fancyId :: Int -> String
fancyId n | n <= 9 = show n
          | otherwise = '%':show n

data SpecificAtom = Primitive PrimitiveAtom [Int] | Description AtomExpression [Int]
  deriving (Eq, Ord)

instance Show SpecificAtom where
  show (Primitive prim idx)   = show prim ++ concatMap fancyId idx
  show (Description expr idx) = concat ["[", show expr, "]", concatMap fancyId idx]


newtype AtomImplicitAnd = AtomImplicitAnd [Specification]
  deriving (Eq, Ord)

instance Show AtomImplicitAnd where
  show (AtomImplicitAnd bonds) = concatMap show bonds


newtype AtomExplicitAnd = AtomExplicitAnd [AtomImplicitAnd]
  deriving (Eq, Ord)

instance Show AtomExplicitAnd where
  show (AtomExplicitAnd expr) = intercalate "&" $ map show expr


newtype AtomOr = AtomOr [AtomExplicitAnd]
  deriving (Eq, Ord)

instance Show AtomOr where
  show (AtomOr expr) = intercalate "," $ map show expr


newtype AtomExpression = AtomExpression [AtomOr]
  deriving (Eq, Ord)

instance Show AtomExpression where
  show (AtomExpression expr) = intercalate ";" $ map show expr


data Specification = Explicit Negation PrimitiveAtom
                   | Degree Negation Int
                   | AttachedHydrogens Negation Int
                   | ImplicitHydrogens Negation Int
                   | RingMembership Negation Int
                   | RingSize Negation Int
                   | Valence Negation Int
                   | Connectivity Negation Int
                   | RingConnectivity Negation Int
                   | NegativeCharge Negation Int
                   | PositiveCharge Negation Int
                   | AtomicNumber Negation Int
                   | CounterClockwise Negation
                   | Clockwise Negation
                   | Chirality Negation ChiralityClass Presence
                   | AtomicMass Negation Int
                   | Recursive Negation SMARTS
  deriving (Eq, Ord)

instance Show Specification where
  show (Explicit neg atom) = show neg ++ show atom
  show (Degree neg num) = showSpec neg num "D"
  show (AttachedHydrogens neg num) = showSpec neg num "H"
  show (ImplicitHydrogens neg num) = showSpec neg num "h"
  show (RingMembership neg num) = showSpec neg num "R"
  show (RingSize neg num) = showSpec neg num "r"
  show (Valence neg num) = showSpec neg num "v"
  show (Connectivity neg num) = showSpec neg num "X"
  show (RingConnectivity neg num) = showSpec neg num "x"
  show (NegativeCharge neg num) = showSpec neg num "-"
  show (PositiveCharge neg num) = showSpec neg num "+"
  show (AtomicNumber neg num) = show neg ++ ('#' : show num)
  show (CounterClockwise neg) = show neg ++ "@"
  show (Clockwise neg) = show neg ++ "@@"
  show (Chirality neg chClass pres) = concat [show neg, "@", show chClass, show pres]
  show (AtomicMass neg num) = show neg ++ show num
  show (Recursive neg smarts) = concat [show neg, "$(", show smarts, ")"]

showSpec :: Negation -> Int -> String -> String
showSpec neg num sym | num == 1 = show neg ++ sym
                     | otherwise = concat [show neg, sym, show num]


data ChiralityClass = TH1 |  TH2 |
                      AL1 |  AL2 |  SP1 |  SP2 |  SP3 |
                      TB3 |  TB4 |  TB5 |  TB6 |  TB7 |  TB8 |  TB9 | TB10 | TB11 | TB12 |
                     TB13 | TB14 | TB15 | TB16 | TB17 | TB18 | TB19 | TB1  | TB20 | TB2  |
                      OH4 |  OH5 |  OH6 |  OH7 |  OH8 |  OH9 | OH10 | OH11 | OH12 | OH13 |
                     OH14 | OH15 | OH16 | OH17 | OH18 | OH19 | OH1  | OH20 | OH21 | OH22 |
                     OH23 | OH24 | OH25 | OH26 | OH27 | OH28 | OH29 | OH2  | OH30 | OH3
  deriving (Show, Read, Eq, Ord, Bounded, Enum)


data Negation = Negate | Pass
  deriving (Eq, Ord)

instance Show Negation where
  show Negate = "!"
  show Pass   = ""


data Presence = Present | Unspecified
  deriving (Eq, Ord)

instance Show Presence where
  show Present     = ""
  show Unspecified = "?"


data Bond = Single Negation
          | Double Negation
          | Triple Negation
          | Aromatic Negation
          | Up Negation Presence
          | Down Negation Presence
          | Ring Negation
          | AnyBond Negation
  deriving (Eq, Ord)

instance Show Bond where
  show (Single Negate)     = "!-"
  show (Single Pass)       = ""
  show (Double neg)        = show neg ++ "="
  show (Triple neg)        = show neg ++ "#"
  show (Aromatic neg)      = show neg ++ ":"
  show (Up neg presence)   = show neg ++ ('/' : show presence)
  show (Down neg presence) = show neg ++ ('\\' : show presence)
  show (Ring neg)          = show neg ++ "@"
  show (AnyBond neg)       = show neg ++ "~"


newtype BondImplicitAnd = BondImplicitAnd [Bond]
  deriving (Eq, Ord)

instance Show BondImplicitAnd where
  show (BondImplicitAnd bonds) = concatMap show bonds


newtype BondExplicitAnd = BondExplicitAnd [BondImplicitAnd]
  deriving (Eq, Ord)

instance Show BondExplicitAnd where
  show (BondExplicitAnd expr) = intercalate "&" $ map show expr


newtype BondOr = BondOr [BondExplicitAnd]
  deriving (Eq, Ord)

instance Show BondOr where
  show (BondOr expr) = intercalate "," $ map show expr


newtype BondExpression = BondExpression [BondOr]
  deriving (Eq, Ord)

instance Show BondExpression where
  show (BondExpression expr) = intercalate ";" $ map show expr


data Component = UnitComponent SpecificAtom | ImplicitSingle SpecificAtom Component | CompoundComponent SpecificAtom BondExpression Component
  deriving (Eq, Ord)

instance Show Component where
  show (UnitComponent unit) = show unit
  show (ImplicitSingle unit component) = show unit ++ show component
  show (CompoundComponent unit bond component) = concat [show unit, show bond, show component]


data Branch = Linear BondExpression Component | Compound BondExpression Component [Branch]
  deriving (Eq, Ord)

instance Show Branch where
  show (Linear bond unit) = show bond ++ show unit
  show (Compound bond unit branch) = concat ["(", show bond, show unit, concatMap show branch, ")"]

data SMARTS = SMARTS Component [Branch]
  deriving (Eq, Ord)

instance Show SMARTS where
  show (SMARTS component list) = show component ++ concatMap show list
