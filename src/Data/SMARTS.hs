module Data.SMARTS (SMARTS (..),
                    Component (..),
                    Branch (..),
                    BondExpression (..),
                    AtomExpression (..),
                    Bond (..),
                    BondOr (..),
                    BondExplicitAnd (..),
                    SpecificAtom (..),
                    AtomOr (..),
                    AtomExplicitAnd (..),
                    AtomImplicitAnd (..),
                    Specification (..),
                    Presence (..),
                    Negation (..),
                    ChiralityClass (..),
                    PrimitiveAtom (..),
                    parseSmarts,
                    smartsP,
                    writeSmarts) where

import           Data.SMARTS.Internal.Parser
import           Data.SMARTS.Internal.Types
import           Data.SMARTS.Internal.Writer
import           Data.Text
import           Text.Megaparsec

parseSmarts :: Text -> Maybe SMARTS
parseSmarts = parseMaybe smartsP
