module Data.SMARTS (SMARTS (..),
                    Component (..),
                    Branch (..),
                    BondExpression (..),
                    AtomExpression (..),
                    Bond (..),
                    BondOr (..),
                    BondExplicitAnd (..),
                    BondImplicitAnd (..),
                    SpecificAtom (..),
                    AtomOr (..),
                    AtomExplicitAnd (..),
                    AtomImplicitAnd (..),
                    Specification (..),
                    Presence (..),
                    Negation (..),
                    Chirality (..),
                    PrimitiveAtom (..),
                    RingClosure (..),
                    parseSmarts,
                    smartsP,
                    writeSmarts) where

import           Data.SMARTS.Internal.Parser
import           Data.SMARTS.Internal.Types
import           Data.SMARTS.Internal.Writer
import           Data.SMILES.Atom            (Chirality (..))
import           Data.Text
import           Text.Megaparsec

parseSmarts :: Text -> Maybe SMARTS
parseSmarts = parseMaybe smartsP
