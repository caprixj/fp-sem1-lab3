# Textbook Processor

A Haskell utility to process a programming textbook file. It parses text into strict types (Word, Sentence, Punctuation), normalizes whitespace (tabs/multiple spaces become single spaces), and filters specific words.

## Features
1.  **Normalization**: Replaces tabs and sequences of spaces with a single space.
2.  **Filtering**: Removes all words of a specific length that start with a consonant.
3.  **Modular Design**: Logic is separated from IO.

## Requirements
* GHC
* Cabal

## How to Run

1.  **Build the project:**
    ```bash
    cabal build
    ```

2.  **Run the utility:**
    Provide the input text file and the length of words to remove.
    
    ```bash
    cabal run textbook-processor -- input.txt 4
    ```

    This will read `input.txt`, remove words of length 4 that start with a consonant, and print the result to stdout.