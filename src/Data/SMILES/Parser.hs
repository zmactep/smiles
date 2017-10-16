module Data.SMILES.Parser
  ( smilesP
  ) where

import           Text.Megaparsec         (between, char, count, digitChar, many,
                                          optional, some, try)
import           Text.Megaparsec.Lexer   (integer)
import           Text.Megaparsec.Text    (Parser)

import           Data.SMILES             (ChainToken (..), SMILES (..))
import           Data.SMILES.Atom.Parser (atomP)
import           Data.SMILES.Bond.Parser (bondP)

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
             Just _  -> read <$> count 2 digitChar
             Nothing -> read . pure <$> digitChar

           return $ RingClosure bondMb i
