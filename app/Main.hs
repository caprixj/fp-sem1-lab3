module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Logic (parseTextbook, processTextbook, renderTextbook)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, lenStr] -> do
            case readMaybe lenStr of
                Just len -> runProgram filePath len
                Nothing  -> putStrLn "Error: The second argument must be an integer (word length)."
        _ -> do
            putStrLn "Usage: textbook-processor <file-path> <word-length-to-remove>"
            putStrLn "Example: textbook-processor input.txt 5"

runProgram :: FilePath -> Int -> IO ()
runProgram path len = do
    content <- TIO.readFile path
    putStrLn $ "Processing file: " ++ path
    putStrLn $ "Removing words of length " ++ show len ++ " starting with a consonant..."

    let textbook = parseTextbook content
    let processed = processTextbook len textbook
    let output = renderTextbook processed

    TIO.putStrLn "\n--- DONE ---"

    let outputFilename = "output.txt"
    TIO.writeFile outputFilename output
    putStrLn $ "Result successfully saved to '" ++ outputFilename ++ "'"
