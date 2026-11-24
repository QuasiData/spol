{-# LANGUAGE OverloadedStrings #-}

module Lexer (Token (..), TokenKind (..), isTrivia, lexer) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

data TokenKind
    = ErrorToken
    | EofToken
    | LParenToken
    | RParenToken
    | LCurlyToken
    | RCurlyToken
    | EqToken
    | SemiToken
    | CommaToken
    | ColonToken
    | ArrowToken
    | PlusToken
    | MinusToken
    | StarToken
    | SlashToken
    | FnKeywordToken
    | LetKeywordToken
    | ReturnKeywordToken
    | TrueKeywordToken
    | FalseKeywordToken
    | NameToken
    | IntToken
    | WhiteSpaceToken
    | CommentToken
    deriving (Show, Eq)

data Token = Token
    { tokenKind :: TokenKind
    , tokenText :: Text
    }
    deriving (Show, Eq)

nameOrKeyword :: Text -> TokenKind
nameOrKeyword "fn" = FnKeywordToken
nameOrKeyword "let" = LetKeywordToken
nameOrKeyword "return" = ReturnKeywordToken
nameOrKeyword "true" = TrueKeywordToken
nameOrKeyword "false" = FalseKeywordToken
nameOrKeyword _ = NameToken

isNameCont :: Char -> Bool
isNameCont c = isAlphaNum c || (c == '_')

isTrivia :: TokenKind -> Bool
isTrivia WhiteSpaceToken = True
isTrivia CommentToken = True
isTrivia _ = False

lexer :: Text -> Vector Token
lexer input = V.fromList $ reverse $ go input []
  where
    go txt tokens = case T.uncons txt of
        Just (c, rest)
            | isSpace c ->
                let (space, rest') = T.span isSpace txt
                 in push WhiteSpaceToken space rest' tokens
            | c == '(' -> push LParenToken (T.take 1 txt) rest tokens
            | c == ')' -> push RParenToken (T.take 1 txt) rest tokens
            | c == '{' -> push LCurlyToken (T.take 1 txt) rest tokens
            | c == '}' -> push RCurlyToken (T.take 1 txt) rest tokens
            | c == '=' -> push EqToken (T.take 1 txt) rest tokens
            | c == ';' -> push SemiToken (T.take 1 txt) rest tokens
            | c == ',' -> push CommaToken (T.take 1 txt) rest tokens
            | c == ':' -> push ColonToken (T.take 1 txt) rest tokens
            | c == '-' -> case T.uncons rest of
                Just (c', rest')
                    | c' == '>' -> push ArrowToken (T.take 2 txt) rest' tokens
                _ -> push MinusToken (T.take 1 txt) rest tokens
            | c == '+' -> push PlusToken (T.take 1 txt) rest tokens
            | c == '*' -> push StarToken (T.take 1 txt) rest tokens
            | c == '/' -> push SlashToken (T.take 1 txt) rest tokens
            | isAlpha c ->
                let (name, rest') = T.span isNameCont txt
                    kind = nameOrKeyword name
                 in push kind (name) rest' tokens
            | isDigit c ->
                let (digit, rest') = T.span isDigit txt
                 in push IntToken digit rest' tokens
            | otherwise -> error "hello"
        Nothing -> tokens

    push kind lexeme rest tokens =
        go rest (Token kind lexeme : tokens)
