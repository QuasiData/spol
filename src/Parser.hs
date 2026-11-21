{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad.ST (ST)
import Control.Monad.State (StateT, get, gets, put)
import Data.List ((!?))
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MV
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

data MarkOpened = MarkOpened {markIndex :: Int}
    deriving (Show, Eq)

data ParserState s = ParserState
    { parserTokens :: [Token]
    , parserFuel :: !Word32
    , parserEvents :: MVector s Event
    , parserEventCounts :: !Int
    }

type Parser s a = StateT (ParserState s) (ST s) a

grow :: Parser s ()
grow = do
    st <- get
    let i = parserEventCounts st
        buf = parserEvents st

    buf' <-
        if i < MV.length buf
            then pure buf
            else MV.grow buf (max 1 (MV.length buf))

    put st{parserEvents = buf'}

open :: Parser s MarkOpened
open = do
    grow

    st <- get
    let buf = parserEvents st
        i = parserEventCounts st

    MV.write buf i (Open ErrorTree)
    put st{parserEventCounts = i + 1}
    pure $ MarkOpened i

close :: MarkOpened -> TreeKind -> Parser s ()
close mark tk = do
    grow

    st <- get
    let buf = parserEvents st
        i = parserEventCounts st
        e = markIndex mark

    MV.write buf e (Open tk)
    MV.write buf i Close

    put st{parserEventCounts = i + 1}

advance :: Parser s ()
advance = do
    grow

    st <- get
    let buf = parserEvents st
        i = parserEventCounts st

    MV.write buf i Advance

    case parserTokens st of
        [] -> error "Called advance on parser with empty tokens"
        _ : ts -> put st{parserTokens = ts, parserFuel = 256, parserEventCounts = i + 1}

eof :: Parser s Bool
eof = do
    tokens <- gets parserTokens
    if null tokens then pure True else pure False

nth :: Int -> Parser s TokenKind
nth lookahead = do
    st <- get

    let fuel = parserFuel st
        tokens = parserTokens st

    if fuel == 0
        then error "Parser ran out of fuel"
        else case tokens !? lookahead of
            Just tok -> pure $ tokenKind tok
            Nothing -> pure $ Eof

at :: TokenKind -> Parser s Bool
at tk = gets $ \st ->
    case parserTokens st of
        [] -> False
        t : _ -> tokenKind t == tk

eat :: TokenKind -> Parser s Bool
eat tk = do
    b <- at tk
    if b
        then do
            advance
            pure True
        else pure False

expect :: TokenKind -> Parser s ()
expect tk = do
    b <- eat tk
    if b
        then pure ()
        -- TODO: add proper error reporting
        else do
            pure ()

advanceWithError :: String -> Parser s ()
advanceWithError _ = do
    m <- open
    -- TODO: proper error report
    advance
    close m ErrorTree

buildTree :: Parser s Tree
buildTree = do
    undefined
