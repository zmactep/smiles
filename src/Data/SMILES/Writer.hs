{-# LANGUAGE OverloadedStrings #-}

module Data.SMILES.Writer
  ( writeSmiles
  ) where

import           Data.Text               (Text, pack)
import qualified Data.Text               as T (concat)

import           Data.SMILES             (ChainToken (..), SMILES (..))
import           Data.SMILES.Atom.Writer (writeAtom)
import           Data.SMILES.Bond.Writer (writeBond)

writeToken :: ChainToken -> Text
writeToken (Atom a)        = writeAtom a
writeToken (Bond b)        = writeBond b
writeToken (RingClosure Nothing r)  | r < 10 = pack $ show r
                                    | otherwise = pack $ "%" ++ show r
writeToken (RingClosure (Just b) r) | r < 10 = T.concat [writeBond b, pack $ show r]
                                    | otherwise = T.concat [writeBond b, pack "%", pack $ show r]
writeToken (Branch b)      = T.concat ["(", writeSmiles b, ")"]

writeSmiles :: SMILES -> Text
writeSmiles (SMILES l) = T.concat $ fmap writeToken l
