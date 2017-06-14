{-# LANGUAGE OverloadedStrings #-}

module Data.SMILES.Writer (writeSmiles) where

import           Data.Text               (Text, pack)
import qualified Data.Text               as T (concat)

import           Data.SMILES             (ChainToken (..), SMILES (..))
import           Data.SMILES.Atom.Writer (writeAtom)
import           Data.SMILES.Bond.Writer (writeBond)

writeToken :: ChainToken -> Text
writeToken (Atom a)        = writeAtom a
writeToken (Bond b)        = writeBond b
writeToken (RingClosure r) = pack $ show r
writeToken (Branch b)      = T.concat ["(", writeSmiles b, ")"]

writeSmiles :: SMILES -> Text
writeSmiles (SMILES l) = T.concat $ fmap writeToken l
