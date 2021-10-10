module Main where

import GPML.Prelude
import GPML.Parser
import GPML.Eval

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
                Right ast -> do 
                    putStrLn $ "AST: " <> show ast
                    case run $ runWithInitialFrames $ runError @EvalError $ eval ast of
                        Left e -> putStrLn $ "eval error: " <> show e
                        Right res -> print res
