-- file: ch05/PrettyStub.hs
import RW.CH5.SimpleJSON
import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char

data Doc = ToBeDefined
         deriving (Show)

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined


string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscaples of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
   where mustEscape c = c < ' ' ||  c == '\x7f' || c > '\xff'

simpleEscaples :: [(Char, String)]
simpleEscaples = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b])

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c


char :: Char -> Doc
char = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

smallHex :: Int -> Doc
smallHex x = text "\\u"
             <> text (replicate (4 - length h) '0')
             <> text h
  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff



