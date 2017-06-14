{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import           Data.SMILES             (SMILES (..))
import qualified Data.SMILES             as S (ChainToken (..))
import           Data.SMILES.Atom
import           Data.SMILES.Atom.Parser
import           Data.SMILES.Parser      (smilesP)
import           Data.SMILES.Writer      (writeSmiles)

import           Data.Text               (Text, pack)
import           Text.Megaparsec         (parseMaybe, some)

parseAtom :: Text -> Maybe Atom
parseAtom = parseMaybe atomP

parseAtomChain :: Text -> Maybe [Atom]
parseAtomChain = parseMaybe (some atomP)

parseSmiles :: Text -> Maybe SMILES
parseSmiles = parseMaybe smilesP

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

moleculeSpecSep :: Spec
moleculeSpecSep =
  describe "Molecule parsing and writing separately" $ do
    it "parses aliphatic linear molecules" $ do
      parseSmiles "CCC" `shouldBe` (Just $ SMILES (replicate 3 (S.Atom $ ali C)))
      parseSmiles "CCCNCCC" `shouldBe` (Just $ SMILES (replicate 3 (S.Atom $ ali C) ++ [S.Atom $ ali N] ++ replicate 3 (S.Atom $ ali C)))

    it "writes aliphatic linear molecules" $ do
      writeSmiles (SMILES $ replicate 3 (S.Atom $ ali C)) `shouldBe` "CCC"
      writeSmiles (SMILES [S.Atom $ ali C, S.Atom $ ali N, S.Atom $ ali N]) `shouldBe` "CNN"

checkSpecSim :: Text -> Expectation
checkSpecSim smi = fmap writeSmiles (parseSmiles smi) `shouldBe` Just smi

moleculeSpecSim :: Spec
moleculeSpecSim =
  describe "Molecule parsing/writing simultaneously" $ do
    it "aliphatic linear molecules" $ do
      checkSpecSim "CCC"
      checkSpecSim "CCCNCCC"

    it "aliphatic branching molecules" $ do
      checkSpecSim "C(C)C(C)C(C)C"
      checkSpecSim "C(C(CC(C)C)C)"

    it "aliphatic cyclic molecules" $ do
      checkSpecSim "C1CCCCC1"
      checkSpecSim "CC(CCCC(C)C)C1CCC2C1(CCC3C2CC=C4C3(CCC(C4)O)C)C"

    it "aromatic molecules" $ do
      checkSpecSim "c1ccccc1"
      checkSpecSim "c1ccc2ccccc2c1"
      checkSpecSim "O=C(O)Cc2c1ccccc1nc2"

    it "chiral molecules" $ do
      checkSpecSim "O[C@@H]1[C@@H](O)[C@@H](OC(O)[C@H]1O)CO"
      checkSpecSim "N[C@@H](Cc1ccc(O)cc1)C(O)=O"
      checkSpecSim "[C@](Cl)(F)(Br)(I)"

    it "complex cases" $ do
      checkSpecSim "C[C@H](Nc1ncc(Cl)c(Nc2cc(C)n[nH]2)n1)c3ncc(F)cn3CC(C)(C)c1cc(CN(C(=O)CN(Cc2ccc(Cl)cc2)S(=O)(=O)c3c(F)c(F)c(F)c(F)c3F)c4ccc(C(=O)O)c(O)c4)cc(c1)C(C)(C)C"
      checkSpecSim "NC(=O)C[C@@H]1NC(=O)C2(CCCCC2)NC(=O)[C@H](Cc3ccc(CP(=O)(O)O)cc3)NC(=O)Cn4cc(CCCNC(=O)[C@H](CC(=O)N)NC(=O)C5(CCCCC5)NC(=O)[C@H](Cc6ccc(CP(=O)(O)O)cc6)NC(=O)Cn7cc(CCCNC1=O)nn7)nn4"
      checkSpecSim "C[C@@H]1O[C@@H](OC[C@H]2O[C@@H](O[C@@](C)(CCC(=O)C(=C)C)[C@H]3CC[C@]4(C)[C@@H]3[C@H](O)C[C@@H]5[C@@]6(C)C[C@@H](O)[C@H](O[C@@H]7O[C@H](CO)[C@@H](O)[C@H](O)[C@H]7O[C@@H]8O[C@H](CO)[C@@H](O)[C@H](O)[C@H]8O)C(C)(C)[C@@H]6CC[C@@]45C)[C@H](O)[C@@H](O)[C@@H]2O)[C@H](O)[C@H](O)[C@H]1O"

main :: IO ()
main = hspec $ do
  atomSpec
  moleculeSpecSep
  moleculeSpecSim
