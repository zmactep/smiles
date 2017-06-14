{-# LANGUAGE OverloadedStrings #-}

module Data.SMILES.Bond.Writer (writeBond)
                                where

import           Data.Text        (Text)

import           Data.SMILES.Bond

writeBond :: Bond -> Text
writeBond (AliphaticBond SingleBond)      = "-"
writeBond (AliphaticBond DoubleBond)      = "="
writeBond (AliphaticBond TripleBond)      = "#"
writeBond (AliphaticBond QuadrupleBond)   = "$"
writeBond AromaticBond                    = ""  -- Aromatic bond is always redundant
writeBond (DoubleConfiguration BelowBond) = "/"
writeBond (DoubleConfiguration AboveBond) = "\\"
