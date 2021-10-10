module GPML.Parser where

import GPML.Prelude hiding (many, some, (<|>))
import GPML.Types.AST

import qualified Data.Text as T hiding (singleton)
import qualified Data.Text.Lazy.Builder as T

import Data.Char

import Text.Read (read)

import Text.Parsec as P
import Text.Parsec.Pos as P

buildText :: T.Builder -> Text
buildText = fromLazy . T.toLazyText

data LexError = UnexpectedChar Char LexState 
              | UnexpectedEOF LexState
              | UnclosedQuote Int LexState
              deriving (Show, Eq)

data Token = Quoted Text
           | Ident Text
           | Paren Text
           | TIntLit Int
           | TDoubleLit Int
           | UnquoteStart
           | UnquoteEnd
           | QuoteStart
           | QuoteEnd
           deriving (Show, Eq)

data LexState = Default 
              | InQuote T.Builder
              | InIdent T.Builder
              | InIntLit T.Builder
              | InDoubleLit T.Builder
              deriving (Show, Eq)

pattern (:>) :: Char -> Text -> Text
pattern (:>) c rest <- (T.uncons -> Just (c, rest))

infixr 5 :>

pattern EOF :: Text
pattern EOF <- (T.uncons -> Nothing)

{-# COMPLETE (:>), EOF #-}

lex :: forall r. (Members '[Error LexError] r) => Text -> Sem r [Token]
lex = go (InQuote mempty) 0
    where
        go :: LexState -> Int -> Text -> Sem r [Token]
        go (InQuote cs) 0 EOF         
            = pure $ maybeToList $ buildTextWith Quoted cs
        go s@(InQuote _) d EOF
            = throw $ UnclosedQuote d s 
        go (InQuote cs) d ('@' :> '{' :> rest) 
            = buildTextWith Quoted cs +?> UnquoteStart +> go Default (d + 1) rest
        go (InQuote cs) d (c :> rest) 
            | d /= 0 && c == '}' = buildTextWith Quoted cs +?> QuoteEnd +> go Default (d - 1) rest
            | otherwise          = go (InQuote (cs <: c)) d rest
    

        go Default _ EOF 
            = throw $ UnexpectedEOF Default
        go Default d ('}' :> '@' :> rest)
            = UnquoteEnd +> go (InQuote mempty) (d - 1) rest
        go Default d (c :> rest)
            | isSpace c = go Default d rest
            | isDigit c = go (InIntLit (T.singleton c)) d rest
            | isIdent c = go (InIdent (T.singleton c)) d rest
            | isParen c = Paren (one c) +> go Default d rest
            | c == '{'  = QuoteStart +> go (InQuote mempty) (d + 1) rest
            | otherwise = throw $ UnexpectedChar c Default

        go s@(InIdent _) _ EOF
            = throw $ UnexpectedEOF s
        go (InIdent cs) d ('}' :> '@' :> rest)
            = buildTextWith Ident cs +?> UnquoteEnd +> go (InQuote mempty) (d - 1) rest
        go s@(InIdent cs) d (c :> rest)
            | isIdent c = go (InIdent (cs <: c)) d rest
            | isParen c = buildTextWith Ident cs +?> Paren (one c) +> go Default d rest
            | c == '{'  = buildTextWith Ident cs +?> QuoteStart +> go (InQuote mempty) (d + 1) rest
            | isSpace c = buildTextWith Ident cs +?> go Default d rest
            | otherwise = throw $ UnexpectedChar c s

        go s@(InIntLit _) _ EOF
            = throw $ UnexpectedEOF s
        go s@(InIntLit cs) d (c :> rest)
            | isDigit c = go (InIntLit (cs <: c)) d rest
            | isSpace c = buildTextWith (TIntLit . read . toString) cs +?> go Default d rest
            | isParen c = buildTextWith (TIntLit . read . toString) cs +?> Paren (one c) +> go Default d rest
            | otherwise = throw $ UnexpectedChar c s

        go s@(InDoubleLit _) _ EOF
            = throw $ UnexpectedEOF s
        go s@(InDoubleLit cs) d (c :> rest)
            | isDigit c = go (InDoubleLit (cs <: c)) d rest
            | otherwise = throw $ UnexpectedChar c s

        buildTextWith :: (Text -> a) -> T.Builder -> Maybe a
        buildTextWith c b = case buildText b of
            EOF -> Nothing
            t   -> Just (c t) 

        (+>) :: forall a. a -> Sem r [a] -> Sem r [a]
        x +> xs = (x:) <$> xs
        infixr 5 +>

        (+?>) :: forall a. Maybe a -> Sem r [a] -> Sem r [a]
        Nothing  +?> xs = xs
        (Just x) +?> xs = x +> xs
        infixr 5 +?>

        (<:) :: T.Builder -> Char -> T.Builder
        b <: c = b <> T.singleton c

isIdent :: Char -> Bool
isIdent c = isAlphaNum c
         || c `elem` ("+-*/#_.,?=$!%&^<>|'~\\;:"::[Char]) --TODO

isParen :: Char -> Bool
isParen c = c `elem` ("()[]" :: [Char])




type Parser = Parsec [Token] ()

sourcePos :: SourcePos
sourcePos = newPos "undefined" 0 0

token' :: (Token -> Maybe a) -> Parser a
token' = token show (const sourcePos)

quoted :: Parser Text
quoted = "quoted text" <??> token' \case
    Quoted x -> Just x
    _ -> Nothing

ident :: Parser Text
ident = "identifier" <??> token' \case
    Ident x | x /= "define" -> Just x
    _ -> Nothing 

defineTok :: Parser ()
defineTok = "define token" <??> token' \case
    Ident "define" -> Just () 
    _ -> Nothing

paren :: Text -> Parser Text
paren x' = (toString x') <??> token' \case
    Paren x | x == x' -> Just x
    _ -> Nothing

unquoteStart :: Parser ()
unquoteStart = "UnquoteStart" <??> token' \case
    UnquoteStart -> Just ()
    _ -> Nothing

unquoteEnd :: Parser ()
unquoteEnd = "UnquoteEnd" <??> token' \case
    UnquoteEnd -> Just ()
    _ -> Nothing

quoteStart :: Parser ()
quoteStart = "QuoteStart" <??> token' \case
    QuoteStart -> Just ()
    _ -> Nothing

quoteEnd :: Parser ()
quoteEnd = "QuoteEnd" <??> token' \case
    QuoteEnd -> Just ()
    _ -> Nothing


(<??>) :: String -> ParsecT s u m a -> ParsecT s u m a
(<??>) = flip (<?>)
infixr 0 <??>

sourceCode :: Parser Expr
sourceCode = "source code" <??> SourceCode <$> many sourceCodeSeg
    where
        sourceCodeSeg = Quote <$> quoted 
                     <|> UnQuote <$> (unquoteStart *> many1' expr <* unquoteEnd) 

expr :: Parser Expr
expr = "expression" <??> parens (define <|> app)
    <|> var
    <|> quote

define :: Parser Expr
define = "define" <??> defineTok *> 
    (   (DefineVar <$> ident <*> expr)
    <|> (uncurry DefineFun <$> parens ((,) <$> ident <*> many ident) <*> many1' expr)
    )

app :: Parser Expr
app = "application" <??> App
    <$> expr
    <*> many expr

var :: Parser Expr
var = "variable" <??> Var <$> ident 

quote :: Parser Expr
quote = "quote" <??> quoteStart *> sourceCode <* quoteEnd


parens :: Parser a -> Parser a
parens p =  paren "(" *> p <* paren ")"
        <|> paren "[" *> p <* paren "]"

many1' :: Parser a -> Parser (NonEmpty a)
many1' p = (:|) <$> p <*> many p 
