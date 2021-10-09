module Main where

import GPML.Prelude
import GPML.Parser

import Text.Parsec (parse)

main :: IO ()
main = do
    putStrLn "Parsing input:"
    input <- getLine
    case run (runError @LexError (lex input)) of
        Left e -> putStrLn $ "lex error: " <> show e
        Right toks -> do 
            putStrLn "toks:"
            traverse_ print toks  
            case parse sourceCode "" toks of
                Left e' -> putStrLn $ "parse error: " <> show e'
                Right ast -> putStrLn $ "AST: " <> show ast
