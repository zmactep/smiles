module Data.SMARTS
  ( AtomExplicitAnd (..), AtomImplicitAnd (..)
  , AtomExpression (..)
  , AtomFeature (..)
  , AtomOr (..)
  , Bond (..)
  , BondExplicitAnd (..), BondImplicitAnd (..)
  , BondExpression (..)
  , BondOr (..)
  , Branch (..)
  , Chirality (..)
  , Component (..)
  , Equation (..)
  , Negation (..)
  , Presence (..)
  , PrimitiveAtom (..)
  , RingClosure (..)
  , ResVal (..)
  , SMARTS (..)
  , Specification (..)
  , SpecificAtom (..)
  , Variable (..)
  , parseSmarts
  , smartsP
  , writeSmarts
  ) where

import           Data.SMARTS.Internal.Parser (smartsP)
import           Data.SMARTS.Internal.Types  (AtomExplicitAnd (..),
                                              AtomExpression (..),
                                              AtomFeature (..),
                                              AtomImplicitAnd (..), AtomOr (..),
                                              Bond (..), BondExplicitAnd (..),
                                              BondExpression (..),
                                              BondImplicitAnd (..), BondOr (..),
                                              Branch (..), Component (..),
                                              Equation (..), Negation (..),
                                              Presence (..), PrimitiveAtom (..),
                                              ResVal (..), RingClosure (..),
                                              SMARTS (..), SpecificAtom (..),
                                              Specification (..), Variable (..))
import           Data.SMARTS.Internal.Writer (writeSmarts)
import           Data.SMILES.Atom            (Chirality (..))
import           Data.Text                   (Text)
import           Text.Megaparsec             (parseMaybe)

parseSmarts :: Text -> Maybe SMARTS
parseSmarts = parseMaybe smartsP
