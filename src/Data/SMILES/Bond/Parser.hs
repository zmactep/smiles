module Data.SMILES.Bond.Parser
  ( bondP
  ) where

import           Data.SMILES.Bond        (AliphaticBond (..), Bond (..),
                                          DoubleConfiguration (..))
import           Data.SMILES.ParserTypes (Parser)
import           Text.Megaparsec         ((<|>))
import           Text.Megaparsec.Char    (char)

bondP :: Parser Bond
bondP = (AliphaticBond <$> aliphaticBondP) <|>
        (DoubleConfiguration <$> doubleConfigBondP) <|>
        aromaticBondP

doubleConfigBondP :: Parser DoubleConfiguration
doubleConfigBondP = (char '/' >> pure BelowBond) <|>
                    (char '\\' >> pure AboveBond)

aliphaticBondP :: Parser AliphaticBond
aliphaticBondP = single <|> double <|> triple <|> quadru
  where single = char '-' >> pure SingleBond
        double = char '=' >> pure DoubleBond
        triple = char '#' >> pure TripleBond
        quadru = char '$' >> pure QuadrupleBond

aromaticBondP :: Parser Bond
aromaticBondP = char ':' >> pure AromaticBond
