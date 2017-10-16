module Data.SMILES.Atom.Parser
  ( atomP
  ) where

import           Data.Char                  (toLower, toUpper)
import           Data.Text                  (pack)
import           Text.Megaparsec            (choice, optional, try, (<|>))
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Data.SMILES.Atom           (Atom (..), AtomSymbol (..),
                                             Bracket (..), Chirality (..),
                                             Organic (..))
import           Data.SMILES.ParserTypes    (Parser, stringP)

atomP :: Parser Atom
atomP = bracketAtomP <|> (SimpleAtom <$> aliphaticAtomP)
                     <|> (SimpleAtom <$> aromaticAtomP)
                     <|> (SimpleAtom <$> wildcardAtomP)

bracketAtomP :: Parser Atom
bracketAtomP = do
  _ <- char '['
  isotope <- optional decimal
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
  where xs = stringP . show <$> [TH1 .. OH3]

hCountP :: Parser Int
hCountP = do mbNum <- char 'H' >> optional decimal
             case mbNum of
               Just x  -> pure x
               Nothing -> pure 1

chargeP :: Parser Int
chargeP = signedCharge '-' (-1) <|> signedCharge '+' 1
  where signedCharge :: Char -> Int -> Parser Int
        signedCharge c mul = do mbNum <- char c >> optional decimal
                                case mbNum of
                                  Just x  -> pure $ mul * x
                                  Nothing -> pure mul

classP :: Parser Int
classP = char ':' >> decimal

aliphaticAtomP :: Parser AtomSymbol
aliphaticAtomP = AliphaticAtom . read <$> choice aliphatics
  where aliphatics = stringP . show <$> [F .. P]

aromaticAtomP :: Parser AtomSymbol
aromaticAtomP = AromaticAtom . read . fmap toUpper <$> choice aromatics
  where aromatics = stringP . fmap toLower . show <$> [B .. P]

wildcardAtomP :: Parser AtomSymbol
wildcardAtomP = char '*' >> pure WildcardAtom

otherAtomP :: Parser AtomSymbol
otherAtomP = OtherAtom . pack <$> choice (fmap stringP lst)
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
