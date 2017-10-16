module Data.SMARTS
  ( AtomExplicitAnd (..), AtomImplicitAnd (..)
  , AtomExpression (..)
  , AtomOr (..)
  , Bond (..)
  , BondExplicitAnd (..), BondImplicitAnd (..)
  , BondExpression (..)
  , BondOr (..)
  , Branch (..)
  , Chirality (..)
  , Component (..)
  , Negation (..)
  , Presence (..)
  , PrimitiveAtom (..)
  , RingClosure (..)
  , SMARTS (..)
  , Specification (..)
  , SpecificAtom (..)
  , parseSmarts
  , smartsP
  , writeSmarts
  ) where

import           Data.SMARTS.Internal.Parser (smartsP)
import           Data.SMARTS.Internal.Types  (AtomExplicitAnd (..),
                                              AtomExpression (..),
                                              AtomImplicitAnd (..), AtomOr (..),
                                              Bond (..), BondExplicitAnd (..),
                                              BondExpression (..),
                                              BondImplicitAnd (..), BondOr (..),
                                              Branch (..), Component (..),
                                              Negation (..), Presence (..),
                                              PrimitiveAtom (..),
                                              RingClosure (..), SMARTS (..),
                                              SpecificAtom (..),
                                              Specification (..))
import           Data.SMARTS.Internal.Writer (writeSmarts)
import           Data.SMILES.Atom            (Chirality (..))
import           Data.Text                   (Text)
import           Text.Megaparsec             (parseMaybe)

parseSmarts :: Text -> Maybe SMARTS
parseSmarts = parseMaybe smartsP
