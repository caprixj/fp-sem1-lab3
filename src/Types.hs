module Types where

import Data.Text (Text)

newtype Symbol = Symbol Char
  deriving (Show, Eq)

newtype WordToken = WordToken Text
  deriving (Show, Eq)

newtype Punctuation = Punctuation Char
  deriving (Show, Eq)

data TextElement
  = EWord WordToken
  | EPunct Punctuation
  | ESpace
  deriving (Show, Eq)
  
newtype Sentence = Sentence [TextElement]
  deriving (Show, Eq)

newtype Textbook = Textbook [Sentence]
  deriving (Show, Eq)
  