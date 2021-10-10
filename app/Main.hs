module Main where

import GPML.Prelude hiding (putStrLn, print)
import GPML.Parser
import GPML.Eval
import GPML.Types.AST

import Text.Parsec (parse)

import Data.Text.IO (getContents, hPutStrLn, hPutStr)

import System.IO (stderr, stdout)
import System.FilePath

import Options.Applicative as O

putErrorLn :: Text -> IO ()
putErrorLn = hPutStrLn stderr
putOut :: Text -> IO ()
putOut = hPutStr stdout

data GMPLOpts = GMPLOpts {
    file :: FilePath
,   output :: Maybe FilePath
,   debugLex :: Bool
,   debugAST :: Bool
} deriving (Show, Eq)

main :: IO ()
main = runGMPL =<< execParser (info (gmplOpts <**> helper) idm)

gmplOpts :: O.Parser GMPLOpts
gmplOpts = GMPLOpts
    <$> argument str (metavar "SOURCE")
    <*> option (Just <$> str) (short 'o' <> metavar "OUTPUT" <> value Nothing <> help "File to write to")
    <*> switch (long "debug-lex")
    <*> switch (long "debug-ast")

runGMPL :: GMPLOpts -> IO ()
runGMPL GMPLOpts{file, output, debugLex, debugAST} = do
    input <- readFileText file
    case run (runError @LexError (lex input)) of
        Left e -> putErrorLn $ "lex error: " <> show e
        Right toks -> do 
            when debugLex $ putErrorLn "toks:"
            traverse_ (putErrorLn . show) toks  
            case parse sourceCode "" toks of
                Left e' -> putErrorLn $ "parse error: " <> show e'
                Right ast -> do 
                    when debugAST $ putErrorLn $ "AST: " <> show ast
                    case run $ runWithInitialFrames $ runError @EvalError $ eval ast of
                        Right (VString s) -> let outPath = fromMaybe (outPathFrom file) output in writeFileText outPath s
                        Right v -> putErrorLn $ "non-string return: " <> show v
                        Left e -> putErrorLn $ "eval error: " <> show e


{-
>>> outPathFrom "path/file.gpml.ext"
"path/file.ext"
>>> outPathFrom "path/file.ext.gpml"
"path/file.ext"
>>> outPathFrom "path/file.gpml"
"path/file"
>>> outPathFrom "path/file"
"path/file.out"
>>> outPathFrom "path/file.ext"
"path/file.ext.out"
-}
outPathFrom :: FilePath -> FilePath
outPathFrom path
    | ext     == ".gpml" = basePath
    | baseExt == ".gpml" = baseBasePath <> ext
    | otherwise          = path     <> ".out"
    where
        (basePath, ext)         = splitExtension path
        (baseBasePath, baseExt) = splitExtension basePath




