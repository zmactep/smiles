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

    it "incorrect SMILES parsing" $ do
      parseSmiles "C()C" `shouldBe` Nothing
      parseSmiles "=CC" `shouldBe` Nothing
      parseSmiles "C(CCC=)" `shouldBe` Nothing
      parseSmiles "CC==C" `shouldBe` Nothing
      parseSmiles "C(==O)CCC" `shouldBe` Nothing
      parseSmiles "Cl[C@@H](==N)C" `shouldBe` Nothing
      parseSmiles "C(1CC)CCC" `shouldBe` Nothing

checkSpecSim :: Text -> Expectation
checkSpecSim smi = fmap writeSmiles (parseSmiles smi) `shouldBe` Just smi

moleculeSpecSim :: Spec
moleculeSpecSim =
  describe "Molecule parsing/writing simultaneously. " $ do
    it "Aliphatic linear molecules" $ do
      checkSpecSim "CCC"
      checkSpecSim "CCCNCCC"

    it "Aliphatic branching molecules" $ do
      checkSpecSim "C(=C)C(=C)C(C)C"
      checkSpecSim "C(C(CC(C)C)C)"
      checkSpecSim "C(C=CCC)CCCCC=CC"

    it "Aliphatic cyclic molecules" $ do
      checkSpecSim "C1CCCCC1"
      checkSpecSim "CC(CCCC(C)C)C1CCC2C1(CCC3C2CC=C4C3(CCC(C4)O)C)C"

    it "Aromatic molecules" $ do
      checkSpecSim "c1ccccc1"
      checkSpecSim "c1ccc2ccccc2c1"
      checkSpecSim "O=C(O)Cc2c1ccccc1nc2"

    it "Chiral molecules" $ do
      checkSpecSim "O[C@@H]1[C@@H](O)[C@@H](OC(O)[C@H]1O)CO"
      checkSpecSim "N[C@@H](Cc1ccc(O)cc1)C(O)=O"
      checkSpecSim "[C@](Cl)(F)(Br)(I)"
      checkSpecSim "C[C@OH20]C"
      checkSpecSim "C[C@OH1]C"
      checkSpecSim "C[C@OH3]C"
      checkSpecSim "C[C@TB15]C"
      checkSpecSim "C[C@TB15]C"
      checkSpecSim "C[C@TB1]C"
      

    it "Complex cases" $ do
      checkSpecSim "C[C@H](Nc1ncc(Cl)c(Nc2cc(C)n[nH]2)n1)c3ncc(F)cn3CC(C)(C)c1cc(CN(C(=O)CN(Cc2ccc(Cl)cc2)S(=O)(=O)c3c(F)c(F)c(F)c(F)c3F)c4ccc(C(=O)O)c(O)c4)cc(c1)C(C)(C)C"
      checkSpecSim "NC(=O)C[C@@H]1NC(=O)C2(CCCCC2)NC(=O)[C@H](Cc3ccc(CP(=O)(O)O)cc3)NC(=O)Cn4cc(CCCNC(=O)[C@H](CC(=O)N)NC(=O)C5(CCCCC5)NC(=O)[C@H](Cc6ccc(CP(=O)(O)O)cc6)NC(=O)Cn7cc(CCCNC1=O)nn7)nn4"
      checkSpecSim "C[C@@H]1O[C@@H](OC[C@H]2O[C@@H](O[C@@](C)(CCC(=O)C(=C)C)[C@H]3CC[C@]4(C)[C@@H]3[C@H](O)C[C@@H]5[C@@]6(C)C[C@@H](O)[C@H](O[C@@H]7O[C@H](CO)[C@@H](O)[C@H](O)[C@H]7O[C@@H]8O[C@H](CO)[C@@H](O)[C@H](O)[C@H]8O)C(C)(C)[C@@H]6CC[C@@]45C)[C@H](O)[C@@H](O)[C@@H]2O)[C@H](O)[C@H](O)[C@H]1O"

    it "Many cycles" $ do
      checkSpecSim "CCCCCCCCOC(=O)CCCC1(c2ccccc2)[C@]23c4c5c6ccc7c8ccc9c%10ccc%11c%12ccc%13c%14c%12c%12c%11c%10c%10c9c8c(c75)c5c%10c%12c(c54)c%14[C@@]12C%13=CC=C63"
      checkSpecSim "c12c3c4c5c6c1c1c7c2c2c8c3c3c4c4c9c5c5c6c6c%10c%11c%12c%13c%14c%15c%16c%17c%18c%19c%20c%16c%16c%15c%12%10c%10c%16c%12c%20c%15c%19c%16c%19c%18c%18c%17c%14c%14c%13c(c9c9c4c4c3c3c8c8c2c2c7c(c%12c2c%15c8c%16c3c%19c4c%18c%149)c%10c16)c5%11"
      checkSpecSim "Nc1ccc(Nc2ccc3nc4ccc(Nc5ccc6nc7ccc(Nc8ccc9nc%10ccc(Nc%11ccccc%11)cc%10[n+](-c%10ccccc%10)c9c8)cc7[n+](-c7ccccc7)c6c5)cc4[n+](-c4ccccc4)c3c2)cc1"

    it "Bond over a cycle" $ do
      checkSpecSim "[C@@H]12[C@H](CCN(C(=O)CCCc3cc4c(cc3)Cc3c-4cccc3)C1)C(=O)NC2"
      checkSpecSim "c12c(cccc1)CN(C([C@H](c1cn(C)nc1)NC)=O)Cc1ccccc1-2"
      checkSpecSim "C(\\c1oc(-c2ccc(C(C)=O)cc2)cc1)=c1\\sc2n(c1=O)[C@@H](c1cccc(OC)c1)C(C(OCC)=O)=C(c1ccccc1)N=2"

    it "Syntactically correct, but semantically wrong" $ do
      checkSpecSim "C1CCCC2"
      checkSpecSim "[216C@@H2+](C)(C)(C)CcC"
      checkSpecSim "C#C#C#[12CH]"

main :: IO ()
main = hspec $ do
  atomSpec
  moleculeSpecSep
  moleculeSpecSim
