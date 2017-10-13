module Data.SMILES.Parser where

import           Data.SMILES
import           Data.SMILES.Atom.Parser
import           Data.SMILES.Bond.Parser
import           Data.SMILES.ParserTypes    (Parser)
import           Text.Megaparsec
import           Text.Megaparsec.Char       (char, digitChar)
import           Text.Megaparsec.Char.Lexer (decimal)

smilesP :: Parser SMILES
smilesP = do atom <- atomPackP
             rest <- concat <$> many chainPackP
             pure $ SMILES (atom ++ rest)

atomPackP :: Parser [ChainToken]
atomPackP = do atom <- Atom <$> atomP
               ringBonds <- many $ try ringP
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
ringP = do bondMb <- optional bondP
           pcMb <- optional $ char '%'
           i <- case pcMb of
             Just _  -> decimal
             Nothing -> read . pure <$> digitChar

           return $ RingClosure bondMb i
