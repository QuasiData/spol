{-# LANGUAGE OverloadedStrings #-}

module Lexer (Token (..), TokenKind (..), lexer) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T

data TokenKind
    = ErrorToken
    | Eof
    | LParen
    | RParen
    | LCurly
    | RCurly
    | Eq
    | Semi
    | Comma
    | Colon
    | Arrow
    | Plus
    | Minus
    | Star
    | Slash
    | FnKeyword
    | LetKeyword
    | ReturnKeyword
    | TrueKeyword
    | FalseKeyword
    | Name
    | Int
    | WhiteSpace
    | Comment
    deriving (Show, Eq)

data Token = Token
    { tokenKind :: TokenKind
    , tokenText :: Text
    }
    deriving (Show, Eq)

nameOrKeyword :: Text -> TokenKind
nameOrKeyword "fn" = FnKeyword
nameOrKeyword "let" = LetKeyword
nameOrKeyword "return" = ReturnKeyword
nameOrKeyword "true" = TrueKeyword
nameOrKeyword "false" = FalseKeyword
nameOrKeyword _ = Name

isNameCont :: Char -> Bool
isNameCont c = isAlphaNum c || (c == '_')

lexer :: Text -> [Token]
lexer input = reverse $ go input []
  where
    go txt tokens = case T.uncons txt of
        Just (c, rest)
            | isSpace c ->
                let (space, rest') = T.span isSpace txt
                 in push WhiteSpace space rest' tokens
            | c == '(' -> push LParen (T.take 1 txt) rest tokens
            | c == ')' -> push RParen (T.take 1 txt) rest tokens
            | c == '{' -> push LCurly (T.take 1 txt) rest tokens
            | c == '}' -> push RCurly (T.take 1 txt) rest tokens
            | c == '=' -> push Eq (T.take 1 txt) rest tokens
            | c == ';' -> push Semi (T.take 1 txt) rest tokens
            | c == ',' -> push Comma (T.take 1 txt) rest tokens
            | c == ':' -> push Colon (T.take 1 txt) rest tokens
            | c == '-' -> case T.uncons rest of
                Just (c', rest')
                    | c' == '>' -> push Arrow (T.take 2 txt) rest' tokens
                _ -> push Minus (T.take 1 txt) rest tokens
            | c == '+' -> push Plus (T.take 1 txt) rest tokens
            | c == '*' -> push Star (T.take 1 txt) rest tokens
            | c == '/' -> push Slash (T.take 1 txt) rest tokens
            | isAlpha c ->
                let (name, rest') = T.span isNameCont txt
                    kind = nameOrKeyword name
                 in push kind (name) rest' tokens
            | isDigit c ->
                let (digit, rest') = T.span isDigit txt
                 in push Int digit rest' tokens
            | otherwise -> error "hello"
        Nothing -> (Token Eof "") : tokens

    push kind lexeme rest tokens =
        go rest (Token kind lexeme : tokens)
