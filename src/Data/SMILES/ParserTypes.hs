{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.SMILES.ParserTypes
  ( Parser
  , stringP
  ) where

import           Data.Text            (Text, pack, unpack)
import           Data.Void            (Void)
import           Text.Megaparsec      (MonadParsec, Parsec, Tokens)
import           Text.Megaparsec.Char (string)

type Parser = Parsec Void Text

stringP :: (Tokens s ~ Text, MonadParsec e s f) => String -> f String
stringP = fmap unpack . string . pack
