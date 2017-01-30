import qualified Data.Char as Char

sumDigits = sum . map Char.digitToInt . show