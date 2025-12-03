module Utils where

import Data.Char (toLower, isAlpha)

-- Includes English and Ukrainian consonants.
isConsonant :: Char -> Bool
isConsonant c
  | not (isAlpha c) = False
  | otherwise =
      let lowerC = toLower c
      in lowerC `notElem` vowels
  where
    vowels = "aeiouy" ++ "аеєиіїоуюя"

isPunctuationChar :: Char -> Bool
isPunctuationChar c = not (isAlpha c) && c /= ' ' && c /= '\t' && c /= '\n'

isSeparator :: Char -> Bool
isSeparator c = c == ' ' || c == '\t' || c == '\n'
