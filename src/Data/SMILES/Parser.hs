module Data.SMILES.Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Lexer
import           Text.Megaparsec.Text

import           Data.SMILES
import           Data.SMILES.Atom.Parser
import           Data.SMILES.Bond.Parser

smilesP :: Parser SMILES
smilesP = do atom <- atomPackP
             rest <- concat <$> many chainPackP
             pure $ SMILES (atom ++ rest)

atomPackP :: Parser [ChainToken]
atomPackP = do atom <- Atom <$> atomP
               ringBonds <- many ringP
               branches <- many branchP
               pure $ (atom : ringBonds) ++ branches

chainPackP :: Parser [ChainToken]
chainPackP = do bondMb <- optional (Bond <$> bondP)
                atomPacks <- concat <$> some atomPackP
                rest <- concat <$> many chainPackP
                case bondMb of
                  Nothing   -> pure $ atomPacks ++ rest
                  Just bond -> pure $ bond : atomPacks ++ rest

branchP :: Parser ChainToken
branchP = Branch <$> between (char '(') (char ')') (SMILES <$> chainPackP)

ringP :: Parser ChainToken
ringP = RingClosure <$> ringHelperP
  where ringHelperP :: Parser Int
        ringHelperP = do pcMb <- optional $ char '%'
                         case pcMb of
                           Just _  -> fromIntegral <$> integer
                           Nothing -> read . pure <$> digitChar
