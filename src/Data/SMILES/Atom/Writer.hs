{-# LANGUAGE OverloadedStrings #-}

module Data.SMILES.Atom.Writer (writeAtom)
                                where

import           Data.Text        (Text, pack)

import           Data.SMILES.Atom

writeChirality :: Chirality -> String
writeChirality Clockwise     = "@@"
writeChirality AntiClockwise = "@"
writeChirality c             = '@':show c

writeHs :: Int -> String
writeHs 0 = ""
writeHs 1 = "H"
writeHs n = "H" ++ show n

writeCharge :: Int -> String
writeCharge 0    = ""
writeCharge (-1) = "-"
writeCharge 1    = "+"
writeCharge n    = show n

writeClass :: Int -> String
writeClass n = ":" ++ show n

writeAtom :: Atom -> Text
writeAtom (BracketAtom Bracket {
  bracketSymbol=sym
  , bracketIsotope=iso
  , bracketChirality=chi
  , bracketHCount=hs
  , bracketCharge=ch
  , bracketClass=cls
}) = pack $ "[" ++
  maybe "" show iso ++
  show sym ++
  maybe "" writeChirality chi ++
  maybe "" writeHs hs ++
  maybe "" writeCharge ch ++
  maybe "" writeClass cls ++
  "]"
writeAtom (SimpleAtom sym) = pack $ show sym
