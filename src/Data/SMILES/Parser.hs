module Data.SMILES.Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Lexer
import           Text.Megaparsec.Text

import           Data.SMILES
import           Data.SMILES.Atom.Parser
import           Data.SMILES.Bond.Parser

smilesP :: Parser SMILES
smilesP = do atom <- Atom <$> atomP
             rest <- many chainTokenP
             pure $ SMILES (atom:rest)

chainTokenP :: Parser ChainToken
chainTokenP = (Atom <$> atomP) <|> (Bond <$> bondP) <|> ringP <|> branchP

branchP :: Parser ChainToken
branchP = Branch <$> between (char '(') (char ')') branchHelperP
  where branchHelperP = do bondMb <- optional (Bond <$> bondP)
                           SMILES rest <- smilesP
                           case bondMb of
                             Just bond -> pure $ SMILES (bond:rest)
                             Nothing   -> pure $ SMILES rest

ringP :: Parser ChainToken
ringP = RingClosure <$> ringHelperP
  where ringHelperP :: Parser Int
        ringHelperP = do pcMb <- optional $ char '%'
                         case pcMb of
                           Just _  -> fromIntegral <$> integer
                           Nothing -> read . pure <$> digitChar
