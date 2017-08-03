{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import Data.SMARTS
import Text.Megaparsec
import Data.Text

main :: IO ()
main = hspec $ do
  basicTests
  specificAtomTests
  stolenFromSmilesTests
  tediousTests

testSuite :: String -> Expectation
testSuite target = smarts `shouldSatisfy` smartsCmp target
  where
    result = parse parseSmarts "" (pack target)
    smarts = case result of
      Right syntaxTree -> writeSmarts syntaxTree
      Left err -> show err

thereAndBackAgain :: String -> Spec
thereAndBackAgain target = it target $ testSuite target

smartsCmp :: String -> String -> Bool
smartsCmp [] [] = True
smartsCmp [] (_:_) = False
smartsCmp (_:_) [] = False
smartsCmp (x:xs) (y:ys) | x == y = smartsCmp xs ys
                        | (x == '1') || (x == '-') = smartsCmp xs (y:ys)
                        | otherwise = False

basicTests :: Spec
basicTests = describe "Simple syntax constructions." $ do
  thereAndBackAgain "a"
  thereAndBackAgain "[a]"
  thereAndBackAgain "[#42]"
  thereAndBackAgain "C"
  thereAndBackAgain "[C@]"
  thereAndBackAgain "[C@@]"
  thereAndBackAgain "CCC"
  thereAndBackAgain "CaCaCaCa"
  thereAndBackAgain "C-C"
  thereAndBackAgain "C-[C]"
  thereAndBackAgain "C=C"
  thereAndBackAgain "C#C"
  thereAndBackAgain "C!#C"
  thereAndBackAgain "C!=C"
  thereAndBackAgain "C!-C"
  thereAndBackAgain "C[Cu]"
  thereAndBackAgain "[F,Cl,Br,I]"
  thereAndBackAgain "[F,Cl;Br&I]"
  thereAndBackAgain "[H+][2H][H][H]"
  thereAndBackAgain "[!#1]"
  thereAndBackAgain "C[C@OH20]C"
  thereAndBackAgain "C[C@OH1]C"
  thereAndBackAgain "C[C@OH3]C"
  thereAndBackAgain "C[C@TB15]C"
  thereAndBackAgain "C[C@TB15]C"
  thereAndBackAgain "C[C@TB1]C"

specificAtomTests :: Spec
specificAtomTests = describe "Various atom description patterns." $ do
  thereAndBackAgain "[$([CX2](=C)=C)]"
  thereAndBackAgain "[$([CX3]=[CX3])]"
  thereAndBackAgain "[$([CX2]#C)]"
  thereAndBackAgain "[$([CX3]=[OX1]),$([CX3+]-[OX1-])]"
  thereAndBackAgain "[CX3](=[OX1])[F,Cl,Br,I]"
  thereAndBackAgain "[CX3H1](=O)[#6]"
  thereAndBackAgain "[CX3](=[OX1])[OX2][CX3](=[OX1])"
  thereAndBackAgain "[NX3][CX3](=[OX1])[#6]"
  thereAndBackAgain "[NX3][CX3]=[NX3+]"
  thereAndBackAgain "[NX3,NX4+][CX3](=[OX1])[OX2,OX1-]"
  thereAndBackAgain "[NX3][CX3](=[OX1])[OX2H0]"
  thereAndBackAgain "[NX3,NX4+][CX3](=[OX1])[OX2H,OX1-]"
  thereAndBackAgain "[CX3](=[OX1])([OX2])[OX2H,OX1H0-1]"
  thereAndBackAgain "[#6][CX3](=O)[OX2H0][#6]"
  thereAndBackAgain "[NX3;H2,H1;!$(NC=O)]"
  thereAndBackAgain "[NX3;H2;!$(NC=[!#6]);!$(NC#[!#6])][#6]"
  thereAndBackAgain "[NX3][$(C=C),$(cc)]"
  thereAndBackAgain "[#7++,17O,S&v3&-1;H2]=[Au][Ac][Ca]"
  thereAndBackAgain "cno[!+2,!+,!-&H1;$([C,N,O,F]=*=*=[At])]"
  thereAndBackAgain "[C,S](=[O,S,P])-[O;H1,-1]"
  thereAndBackAgain "[C!@OH21?$(C/?C\\o#o:P)]"

stolenFromSmilesTests :: Spec
stolenFromSmilesTests = describe "SMILES string should be a valid SMARTS pattern." $ do
  it "Aliphatic linear molecules" $ do
    testSuite "CCC"
    testSuite "CCCNCCC"

  it "Aliphatic branching molecules" $ do
    testSuite "C(=C)C(=C)C(C)C"
    testSuite "C(C(CC(C)C)C)"
    testSuite "C(C=CCC)CCCCC=CC"

  it "Aliphatic cyclic molecules" $ do
    testSuite "C1CCCCC1"
    testSuite "CC(CCCC(C)C)C1CCC2C1(CCC3C2CC=C4C3(CCC(C4)O)C)C"

  it "Aromatic molecules" $ do
    testSuite "c1ccccc1"
    testSuite "c1ccc2ccccc2c1"
    testSuite "O=C(O)Cc2c1ccccc1nc2"

  it "Chiral molecules" $ do
    testSuite "O[C@@H]1[C@@H](O)[C@@H](OC(O)[C@H]1O)CO"
    testSuite "N[C@@H](Cc1ccc(O)cc1)C(O)=O"
    testSuite "[C@](Cl)(F)(Br)(I)"

  it "Complex cases" $ do
    testSuite "C[C@H](Nc1ncc(Cl)c(Nc2cc(C)n[nH]2)n1)c3ncc(F)cn3CC(C)(C)c1cc(CN(C(=O)CN(Cc2ccc(Cl)cc2)S(=O)(=O)c3c(F)c(F)c(F)c(F)c3F)c4ccc(C(=O)O)c(O)c4)cc(c1)C(C)(C)C"
    testSuite "NC(=O)C[C@@H]1NC(=O)C2(CCCCC2)NC(=O)[C@H](Cc3ccc(CP(=O)(O)O)cc3)NC(=O)Cn4cc(CCCNC(=O)[C@H](CC(=O)N)NC(=O)C5(CCCCC5)NC(=O)[C@H](Cc6ccc(CP(=O)(O)O)cc6)NC(=O)Cn7cc(CCCNC1=O)nn7)nn4"
    testSuite "C[C@@H]1O[C@@H](OC[C@H]2O[C@@H](O[C@@](C)(CCC(=O)C(=C)C)[C@H]3CC[C@]4(C)[C@@H]3[C@H](O)C[C@@H]5[C@@]6(C)C[C@@H](O)[C@H](O[C@@H]7O[C@H](CO)[C@@H](O)[C@H](O)[C@H]7O[C@@H]8O[C@H](CO)[C@@H](O)[C@H](O)[C@H]8O)C(C)(C)[C@@H]6CC[C@@]45C)[C@H](O)[C@@H](O)[C@@H]2O)[C@H](O)[C@H](O)[C@H]1O"

  it "Many cycles" $ do
    testSuite "CCCCCCCCOC(=O)CCCC1(c2ccccc2)[C@]23c4c5c6ccc7c8ccc9c%10ccc%11c%12ccc%13c%14c%12c%12c%11c%10c%10c9c8c(c75)c5c%10c%12c(c54)c%14[C@@]12C%13=CC=C63"
    testSuite "c12c3c4c5c6c1c1c7c2c2c8c3c3c4c4c9c5c5c6c6c%10c%11c%12c%13c%14c%15c%16c%17c%18c%19c%20c%16c%16c%15c%12%10c%10c%16c%12c%20c%15c%19c%16c%19c%18c%18c%17c%14c%14c%13c(c9c9c4c4c3c3c8c8c2c2c7c(c%12c2c%15c8c%16c3c%19c4c%18c%149)c%10c16)c5%11"
    testSuite "Nc1ccc(Nc2ccc3nc4ccc(Nc5ccc6nc7ccc(Nc8ccc9nc%10ccc(Nc%11ccccc%11)cc%10[n+](-c%10ccccc%10)c9c8)cc7[n+](-c7ccccc7)c6c5)cc4[n+](-c4ccccc4)c3c2)cc1"

  it "Syntactically correct, but semantically wrong" $ do
    testSuite "C1CCCC2"
    testSuite "[216C@@H2+](C)(C)(C)CcC"
    testSuite "C#C#C#[12CH]"

tediousTests :: Spec
tediousTests = describe "Complex patterns including resursive SMARTS, disconnected components etc." $ do
  thereAndBackAgain "[$([NX3H2,NX4H3+]),$([NX3H](C)(C))][CX4H]([*])[CX3](=[OX1])[OX2H,OX1-,N]"
  thereAndBackAgain "[$([$([NX3H2,NX4H3+]),$([NX3H](C)(C))][CX4H2][CX3](=[OX1])[OX2H,OX1-,N])]"
  thereAndBackAgain "[CH2X4][#6X3]1:[$([#7X3H+,#7X2H0+0]:[#6X3H]:[#7X3H]),$([#7X3H])]:[#6X3H]:[$([#7X3H+,#7X2H0+0]:[#6X3H]:[#7X3H]),$([#7X3H])]:[#6X3H]1"
  thereAndBackAgain "[$([C,S](=[O,S,P])-[O;H1,-1])]"
  thereAndBackAgain "[$([N;!H0;v3,v4&+1]),$([O,S;H1;+0]),n&H1&+0]"
  thereAndBackAgain "[$([O,S;H1;v2;!$(*-*=[O,N,P,S])]),$([O,S;H0;v2]),$([O,S;-]),$([N;v3;!$(N-*=[O,N,P,S])]),n&H0&+0,$([o,s;+0;!$([o,s]:n);!$([o,s]:c:n)])]"
  thereAndBackAgain "[#7;+,$([N;H2&+0][$([C,a]);!$([C,a](=O))]),$([N;H1&+0]([$([C,a]);!$([C,a](=O))])[$([C,a]);!$([C,a](=O))]),$([N;H0&+0]([C;!$(C(=O))])([C;!$(C(=O))])[C;!$(C(=O))])]"
