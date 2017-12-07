{-# LANGUAGE RecordWildCards #-}

module Data.SMARTS.Internal.Types where

import           Data.List        (intercalate)
import           Data.SMILES.Atom (Chirality)
import           Data.Text        (Text, unpack)
import           Text.Printf      (printf)

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


data RingClosure = Closure BondExpression Int
  deriving (Eq, Ord)

instance Show RingClosure where
  show (Closure bond idx) = show bond ++ fancyId idx


data SpecificAtom = Primitive PrimitiveAtom [RingClosure] | Description AtomExpression [RingClosure]
  deriving (Eq, Ord)

instance Show SpecificAtom where
  show (Primitive prim idx)   = show prim ++ concatMap show idx
  show (Description expr idx) = concat ["[", show expr, "]", concatMap show idx]


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


data AtomExpression = AtomExpression Negation [AtomOr]
  deriving (Eq, Ord)

instance Show AtomExpression where
  show (AtomExpression neg expr) | neg == Pass = strRepr
                                 | otherwise   = "!{" ++ strRepr ++ "}2"
    where
      strRepr = intercalate ";" $ map show expr

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
                   | CounterClockwise Negation Presence
                   | ClockwiseCh Negation
                   | ChiralityClass Negation Chirality Presence
                   | AtomicMass Negation Int
                   | ArylGroup Negation
                   | HeteroarylGroup Negation
                   | AromaticNeighbours Negation Int
                   | AcidityInterval Float Float
                   | BasicityInterval Float Float
                   | ChargeInterval Float Float
                   | Recursive Negation SMARTS
                   | Class Int
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
  show (CounterClockwise neg pres) = show neg ++ "@" ++ show pres
  show (ClockwiseCh neg) = show neg ++ "@@"
  show (ChiralityClass neg chClass pres) = concat [show neg, "@", show chClass, show pres]
  show (AtomicMass neg num) = show neg ++ show num
  show (ArylGroup neg) = show neg ++ "AG"
  show (HeteroarylGroup neg) = show neg ++ "HG"
  show (AromaticNeighbours neg num) = show neg ++ "^a" ++ show num
  show (AcidityInterval a1 a2) = "(a" ++ printf "%.3f" a1 ++ "," ++ printf "%.3f" a2 ++ ")"
  show (BasicityInterval b1 b2) = "(b" ++ printf "%.3f" b1 ++ "," ++ printf "%.3f" b2 ++ ")"
  show (ChargeInterval c1 c2) = "(c" ++ printf "%.3f" c1 ++ "," ++ printf "%.3f" c2 ++ ")"
  show (Recursive neg smarts) = concat [show neg, "$(", show smarts, ")"]
  show (Class num) = ':' : show num

showSpec :: Negation -> Int -> String -> String
showSpec neg num sym | (num == 1) || (num < 0) = show neg ++ sym
                     | otherwise = concat [show neg, sym, show num]


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
          | Dative Negation
          | AnyBond Negation
          | Implicit
  deriving (Eq, Ord)

instance Show Bond where
  show (Single neg)        = show neg ++ "-"
  show (Double neg)        = show neg ++ "="
  show (Triple neg)        = show neg ++ "#"
  show (Aromatic neg)      = show neg ++ ":"
  show (Up neg presence)   = show neg ++ ('/' : show presence)
  show (Down neg presence) = show neg ++ ('\\' : show presence)
  show (Ring neg)          = show neg ++ "@"
  show (Dative neg)        = show neg ++ "_"
  show (AnyBond neg)       = show neg ++ "~"
  show Implicit            = ""

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


newtype Component = Component [(BondExpression, SpecificAtom)]
  deriving (Eq, Ord)

instance Show Component where
  show (Component component) = concatMap (\(a, b) -> show a ++ show b) component


data Branch = Linear Component | Compound Component [Branch]
  deriving (Eq, Ord)

instance Show Branch where
  show (Linear unit) = show unit
  show (Compound unit branch) = concat ["(", show unit, concatMap show branch, ")"]

data SMARTS = SMARTS { branches  :: [Branch]
                     , equations :: [Equation]
                     }
  deriving (Eq, Ord)

instance Show SMARTS where
  show SMARTS{..} = concatMap show branches ++ concatMap (\x -> "|" ++ show x) equations

data Equation = Equation [Variable] ResVal deriving (Eq, Ord)

instance Show Equation where
  show (Equation [] _) = ""
  show (Equation (x : xs) resVal) = filter (/= '+') (show x) ++ concatMap show xs ++ ">" ++ show resVal

data AtomFeature = Acidicity | Basicity | Charge deriving (Eq, Ord)

instance Show AtomFeature where
  show Acidicity = "a"
  show Basicity  = "b"
  show Charge    = "c"

data Variable = Variable Float Int AtomFeature deriving (Eq, Ord)

instance Show Variable where
  show (Variable val indOfAtom feature) = addSign ++ printf "%.3f" val ++ "$" ++ show indOfAtom ++ show feature
    where
      addSign = if val > 0 then "+" else ""

newtype ResVal = ResVal Float deriving (Eq, Ord)

instance Show ResVal where
  show (ResVal val) =  printf "%.3f" val
