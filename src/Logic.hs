module Logic 
    ( parseTextbook
    , processTextbook
    , renderTextbook
    ) where

import qualified Data.Text as T
import Data.Text (Text)
import Types
import Utils

parseTextbook :: Text -> Textbook
parseTextbook rawText = 
    let tokens = tokenize (T.unpack rawText)
        sentences = groupSentences tokens
    in Textbook sentences

tokenize :: String -> [TextElement]
tokenize [] = []
tokenize (x:xs)
    | isSeparator x = 
        case xs of
            (y:_) | isSeparator y -> tokenize xs
            _                     -> ESpace : tokenize xs
    | isPunctuationChar x = EPunct (Punctuation x) : tokenize xs
    | otherwise = 
        let (wordChars, rest) = span (\c -> not (isSeparator c) && not (isPunctuationChar c)) (x:xs)
        in EWord (WordToken (T.pack wordChars)) : tokenize rest

groupSentences :: [TextElement] -> [Sentence]
groupSentences [] = []
groupSentences elems =
    let (s, rest) = break isSentenceEnd elems
        (terminator, remaining) = case rest of
            (t:rs) -> ([t], rs)
            []     -> ([], [])
    in Sentence (s ++ terminator) : groupSentences remaining
  where
    isSentenceEnd (EPunct (Punctuation c)) = c `elem` ['.', '!', '?']
    isSentenceEnd _ = False

processTextbook :: Int -> Textbook -> Textbook
processTextbook targetLen (Textbook sentences) = 
    Textbook (map (processSentence targetLen) sentences)

processSentence :: Int -> Sentence -> Sentence
processSentence l (Sentence elems) = Sentence (filter (keepElement l) elems)

keepElement :: Int -> TextElement -> Bool
keepElement l (EWord (WordToken t)) = 
    not (shouldDelete l t)
keepElement _ _ = True

shouldDelete :: Int -> Text -> Bool
shouldDelete l t =
    (T.length t == l) &&
    case T.uncons t of
        Just (h, _) -> isConsonant h
        Nothing     -> False

renderTextbook :: Textbook -> Text
renderTextbook (Textbook sentences) = 
    T.concat $ map renderSentence sentences

renderSentence :: Sentence -> Text
renderSentence (Sentence elems) = 
    T.concat $ map renderElement elems

renderElement :: TextElement -> Text
renderElement (EWord (WordToken t))   = t
renderElement (EPunct (Punctuation c)) = T.singleton c
renderElement ESpace                  = " "
