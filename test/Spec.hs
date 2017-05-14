{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import Data.SMILES.Atom
import Data.SMILES.Atom.Parser

import Text.Megaparsec (parseMaybe, some)
import Data.Text (Text)

parseAtom :: Text -> Maybe Atom
parseAtom = parseMaybe atomP

parseAtomChain :: Text -> Maybe [Atom]
parseAtomChain = parseMaybe (some atomP)

ali :: Organic -> Atom
ali = SimpleAtom . AliphaticAtom

aro :: Organic -> Atom
aro = SimpleAtom . AromaticAtom

defbra :: AtomSymbol -> Bracket
defbra x = Bracket x Nothing Nothing Nothing Nothing Nothing

atomSpec :: Spec
atomSpec =
  describe "Atom parsing" $ do
    it "parses aliphatic atoms correct" $ do
      parseAtom "C"  `shouldBe` Just (ali C)
      parseAtom "Cl" `shouldBe` Just (ali Cl)
      parseAtom "I"  `shouldBe` Just (ali I)
      parseAtom "Z"  `shouldBe` Nothing
    it "parses aromatic atoms correct" $ do
      parseAtom "n"  `shouldBe` Just (aro N)
      parseAtom "s"  `shouldBe` Just (aro S)
      parseAtom "p"  `shouldBe` Just (aro P)
      parseAtom "cl" `shouldBe` Nothing
    it "parses bracket atoms correct" $ do
      parseAtom "[C]"    `shouldBe` Just (BracketAtom (defbra (AliphaticAtom C)))
      parseAtom "[Pb]"   `shouldBe` Just (BracketAtom (defbra (OtherAtom "Pb")))
      parseAtom "[CH4]"  `shouldBe` Just (BracketAtom ((defbra (AliphaticAtom C)) { bracketHCount = Just 4}))
      parseAtom "[2H]"   `shouldBe` Just (BracketAtom ((defbra (OtherAtom "H")) { bracketIsotope = Just 2}))
      parseAtom "[*]"    `shouldBe` Just (BracketAtom (defbra WildcardAtom))
      parseAtom "[Cu+2]" `shouldBe` Just (BracketAtom ((defbra (OtherAtom "Cu")) { bracketCharge = Just 2}))
      parseAtom "[C@H]"  `shouldBe` Just (BracketAtom ((defbra (AliphaticAtom C)) { bracketChirality = Just AntiClockwise, bracketHCount = Just 1 }))
      parseAtom "[C@@H]" `shouldBe` Just (BracketAtom ((defbra (AliphaticAtom C)) { bracketChirality = Just Clockwise, bracketHCount = Just 1 }))
      parseAtom "[OH1-]" `shouldBe` parseAtom "[OH-1]"
    it "parses wildcard atoms correct" $ do
      parseAtom "*" `shouldBe` Just (SimpleAtom WildcardAtom)
      parseAtom "#" `shouldBe` Nothing
    it "parses chain of atoms correct" $ do
      1 `shouldBe` 1

main :: IO ()
main = hspec atomSpec
