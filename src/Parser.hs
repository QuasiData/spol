{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad (unless, void, when)
import Control.Monad.State (State, get, gets, modify', put)
import Data.Functor (($>))
import Data.List ((!?))
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as S
import Data.Word (Word32)
import Lexer

data TreeKind
    = ErrorTree
    | File
    | Fn
    | TypeExpr
    | ParamList
    | Param
    | Block
    | StmtLet
    | StmtReturn
    | StmtExpr
    | ExprLiteral
    | ExprName
    | ExprParen
    | ExprBinary
    | ExprCall
    | ArgList
    | Arg
    deriving (Show, Eq)

data Tree = Tree
    { treeKind :: TreeKind
    , children :: [Child]
    }
    deriving (Show, Eq)

data Child = ChildToken Token | ChildTree Tree
    deriving (Show, Eq)

data Event = Open {eventKind :: TreeKind} | Close | Advance
    deriving (Show, Eq)

data MarkOpened = MarkOpened {openIndex :: Int}
    deriving (Show, Eq)

data MarkClosed = MarkClosed {closedIndex :: Int}
    deriving (Show, Eq)

data ParserState = ParserState
    { parserTokens :: [Token]
    , parserFuel :: !Word32
    , parserEvents :: Seq Event
    }

type Parser a = State ParserState a

emit :: Event -> Parser Int
emit event = do
    st <- get
    let events = parserEvents st
        i = S.length events

    put st{parserEvents = events |> event}
    pure i

open :: Parser MarkOpened
open = MarkOpened <$> emit (Open ErrorTree)

openBefore :: MarkClosed -> Parser MarkOpened
openBefore (MarkClosed i) = do
    modify' $ \st -> st {parserEvents = S.insertAt i (Open ErrorTree) (parserEvents st)}
    pure (MarkOpened i)

close :: MarkOpened -> TreeKind -> Parser MarkClosed
close mark tk = do
    modify' $ \st -> st{parserEvents = S.update (openIndex mark) (Open tk) (parserEvents st)}
    void $ emit Close
    pure $ MarkClosed (openIndex mark)

advance :: Parser ()
advance = do
    tokens <- gets parserTokens
    case tokens of
        [] -> error "Called advance on parser with empty tokens"
        _ : ts -> do
            void $ emit Advance
            modify' $ \st -> st{parserTokens = ts, parserFuel = 256}

withNode :: TreeKind -> Parser a -> Parser a
withNode tk parser = do
    mark <- open
    result <- parser
    _ <- close mark tk
    pure result

withClosedNode :: TreeKind -> Parser () -> Parser MarkClosed
withClosedNode tk p = do
    m <- open
    p
    close m tk

eof :: Parser Bool
eof = gets (null . parserTokens)

nth :: Int -> Parser TokenKind
nth lookahead = do
    st <- get
    when (parserFuel st <= 0) $
        error "Parser ran out of fuel"

    modify' $ \s -> s{parserFuel = parserFuel s - 1}
    pure $ maybe Eof tokenKind (parserTokens st !? lookahead)

at :: TokenKind -> Parser Bool
at tk = (== tk) <$> nth 0

eat :: TokenKind -> Parser Bool
eat tk = do
    found <- at tk
    when found advance
    pure found

expect :: TokenKind -> Parser ()
expect tk = do
    b <- eat tk
    if b
        then pure ()
        -- TODO: add proper error reporting
        else do
            pure ()

advanceWithError :: String -> Parser ()
advanceWithError _ = withNode ErrorTree advance

-- TODO: proper error report

untilM_ :: (Monad m) => m Bool -> m () -> m ()
untilM_ condition action = do
    done <- condition
    unless done $ do
        action
        untilM_ condition action

parseFile :: Parser ()
parseFile =
    withNode File $
        untilM_ eof parseItem

parseItem :: Parser ()
parseItem = do
    isFn <- at FnKeyword
    if isFn
        then parseFunc
        else advanceWithError "Expected a function"

parseFunc :: Parser ()
parseFunc = withNode Fn $ do
    expect FnKeyword
    expect Name

    lparen <- at LParen
    when lparen $ do
        parseParamList

    arrow <- eat Arrow
    when arrow $ do
        parseTypeExpr

    lcurly <- at LCurly
    when lcurly $ do
        parseBlock

parseParamList :: Parser ()
parseParamList = withNode ParamList $ do
    expect LParen
    go
    expect RParen
  where
    go = do
        closing <- at RParen
        end <- eof

        unless (closing || end) $ do
            atName <- at Name
            when atName $ do
                parseParam
                go

parseParam :: Parser ()
parseParam = withNode Param $ do
    expect Name
    expect Colon
    parseTypeExpr
    atRParen <- at RParen
    unless atRParen $ do
        expect Comma

parseTypeExpr :: Parser ()
parseTypeExpr = withNode TypeExpr (expect Name)

parseBlock :: Parser ()
parseBlock = withNode Block $ do
    expect LCurly
    go
    expect RCurly
  where
    go = do
        closing <- at RCurly
        end <- eof

        unless (closing || end) $ do
            next <- nth 0
            case next of
                LetKeyword -> parseStmtLet
                ReturnKeyword -> parseStmtReturn
                _ -> parseStmtExpr
            go

parseStmtLet :: Parser ()
parseStmtLet = withNode StmtLet $ do
    expect LetKeyword
    expect Name
    expect Eq
    parseExpr
    expect Semi

parseStmtReturn :: Parser ()
parseStmtReturn = withNode StmtReturn $ do
    expect ReturnKeyword
    parseExpr
    expect Semi

parseStmtExpr :: Parser ()
parseStmtExpr = withNode StmtExpr $ do
    parseExpr
    expect Semi

parseExpr :: Parser ()
parseExpr = do undefined
        

parseExprDelimited :: Parser MarkClosed
parseExprDelimited = do
    mark <- open
    next <- nth 0
    tk <- case next of
        tk
            | tk `elem` [Int, TrueKeyword, FalseKeyword] ->
                advance $> ExprLiteral
        Name -> advance $> ExprName
        LParen -> do
            advance
            parseExpr
            expect RParen
            pure ExprParen
        _ -> do
            end <- eof
            unless end advance
            pure ErrorTree

    close mark tk

buildTree :: [Token] -> [Event] -> Tree
buildTree ps = undefined
