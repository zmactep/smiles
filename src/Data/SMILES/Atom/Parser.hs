module Data.SMILES.Atom.Parser where

import           Data.Char             (toLower, toUpper)
import           Data.Text             (pack)
import           Text.Megaparsec
import           Text.Megaparsec.Lexer
import           Text.Megaparsec.Text

import           Data.SMILES.Atom

atomP :: Parser Atom
atomP = bracketAtomP <|> (SimpleAtom <$> aliphaticAtomP)
                     <|> (SimpleAtom <$> aromaticAtomP)
                     <|> (SimpleAtom <$> wildcardAtomP)

bracketAtomP :: Parser Atom
bracketAtomP = do
  _ <- char '['
  isotope <- (fromIntegral <$>) <$> optional integer
  element <- otherAtomP <|> aliphaticAtomP <|> aromaticAtomP <|> wildcardAtomP
  chirality <- optional chiralityP
  hCount <- optional hCountP
  charge <- optional chargeP
  class' <- optional classP
  _ <- char ']'
  pure $ BracketAtom $ Bracket element isotope chirality hCount charge class'

chiralityP :: Parser Chirality
chiralityP = try (char '@' >> (char '@' >> pure Clockwise) <|> (read <$> choice xs)) <|>
             (char '@' >> pure AntiClockwise)
  where xs = string . show <$> [TH1 .. OH3]

hCountP :: Parser Int
hCountP = do mbNum <- char 'H' >> optional integer
             case mbNum of
               Just x  -> pure $ fromIntegral x
               Nothing -> pure 1

chargeP :: Parser Int
chargeP = signedCharge '-' (-1) <|> signedCharge '+' 1
  where signedCharge :: Char -> Int -> Parser Int
        signedCharge c mul = do mbNum <- char c >> optional integer
                                case mbNum of
                                  Just x  -> pure $ mul * fromIntegral x
                                  Nothing -> pure mul

classP :: Parser Int
classP = fromIntegral <$> (char ':' >> integer)

aliphaticAtomP :: Parser AtomSymbol
aliphaticAtomP = AliphaticAtom . read <$> choice aliphatics
  where aliphatics = string . show <$> [F .. P]

aromaticAtomP :: Parser AtomSymbol
aromaticAtomP = AromaticAtom . read . fmap toUpper <$> choice aromatics
  where aromatics = string . fmap toLower . show <$> [B .. P]

wildcardAtomP :: Parser AtomSymbol
wildcardAtomP = char '*' >> pure WildcardAtom

otherAtomP :: Parser AtomSymbol
otherAtomP = OtherAtom . pack <$> choice (fmap string lst)
  where lst = ["Zr","Zn","Yb","Y","Xe","W","V","U","Tm","Tl",
               "Ti","Th","Te","Tc","Tb","Ta","Sr","Sn","Sm",
               "Si","Sg","Se","Sc","Sb","Ru","Rn","Rh",
               "Rg","Rf","Re","Rb","Ra","Pu","Pt","Pr","Po",
               "Pm","Pd","Pb","Pa","Os","Np","No","Ni",
               "Ne","Nd","Nb","Na","Mt","Mo","Mn","Mg",
               "Md","Lv","Lu","Lr","Li","La","Kr","K","Ir",
               "In","Hs","Ho","Hg","Hf","He","H","Ge",
               "Gd","Ga","Fr","Fm","Fl","Fe","Eu","Es",
               "Er","Dy","Ds","Db","Cu","Cs","Cr","Co","Cn",
               "Cm","Cf","Ce","Cd","Ca","Bk",
               "Bi","Bh","Be","Ba","Au","At","As","Ar",
               "Am","Al","Ag","Ac","se","as"]
