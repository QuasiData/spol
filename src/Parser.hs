{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Parser where

import Lexer

import Control.Monad (void)
import Control.Monad.ST (ST, runST)
import Control.Monad.State (MonadState (put), StateT, get, gets, runStateT)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MV
import Data.Word (Word32)

data TreeKind
    = ErrorTree
    | FileTree
    | FnTree
    | TypeExprTree
    | ParamListTree
    | ParamTree
    | BlockTree
    | StmtLetTree
    | StmtReturnTree
    | StmtExprTree
    | ExprLiteralTree
    | ExprNameTree
    | ExprParenTree
    | ExprBinaryTree
    | ExprCallTree
    | ArgListTree
    | ArgTree
    deriving (Show, Eq)

data Tree = Tree TreeKind [Tree] | Leaf Token
    deriving (Show, Eq)

data Event
    = Open {eventKind :: TreeKind, forwardParent :: Maybe Int}
    | Close
    | Advance
    deriving (Show, Eq)

data MarkOpened = MarkOpened {openIndex :: Int}
    deriving (Show, Eq)

data MarkClosed = MarkClosed {closedIndex :: Int}
    deriving (Show, Eq)

data ParserState s = ParserState
    { parserTokens :: !(Vector Token)
    , parserCursor :: !Int
    , parserFuel :: !Word32
    , parserEvents :: !(MVector s Event)
    , parserEventCount :: !Int
    }

type Parser s a = StateT (ParserState s) (ST s) a

emit :: Event -> Parser s Int
emit event = do
    st <- get
    let buf = parserEvents st
        n = parserEventCount st

    buf' <-
        if n < MV.length buf
            then pure buf
            else MV.grow buf (max 1 (MV.length buf))

    MV.unsafeWrite buf' n event
    put $! st{parserEvents = buf', parserEventCount = n + 1}
    pure n

open :: Parser s MarkOpened
open = do
    MarkOpened <$> emit (Open ErrorTree Nothing)

close :: MarkOpened -> TreeKind -> Parser s MarkClosed
close (MarkOpened i) tk = do
    buf <- gets parserEvents
    event <- MV.read buf i

    case event of
        Open _ parent -> MV.write buf i (Open tk parent)
        _ -> error "close: marker does not reference and Open event"

    _ <- emit Close
    pure (MarkClosed i)

openBefore :: MarkClosed -> Parser s MarkOpened
openBefore (MarkClosed i) = do
    mark@(MarkOpened j) <- open

    buf <- gets parserEvents
    event <- MV.read buf i

    case event of
        Open tk Nothing -> MV.write buf i (Open tk (Just j))
        _ -> error "openBefore: invalid marker"

    pure mark

nth :: Int -> Parser s TokenKind
nth = undefined -- Decrement fuel

at :: TokenKind -> Parser s Bool
at = undefined

atAny :: [TokenKind] -> Parser s Bool
atAny = undefined

eof :: Parser s Bool
eof = undefined

advance :: Parser s ()
advance = undefined -- Reset fuel

eat :: TokenKind -> Parser s Bool
eat = undefined

expect :: TokenKind -> Parser s ()
expect = undefined

reportError :: Text -> Parser s ()
reportError = undefined

advanceWithError :: Text -> Parser s ()
advanceWithError = undefined

parseFile :: Parser s ()
parseFile = do
    void $ emit Close

buildTree :: Vector Token -> Vector Event -> Tree
buildTree = undefined

parseTest :: (forall s. Parser s a) -> Text -> Vector Event
parseTest parser input = runST $ do
    buffer <- MV.new 128
    let initial =
            ParserState
                { parserTokens = tokens
                , parserCursor = 0
                , parserFuel = 256
                , parserEvents = buffer
                , parserEventCount = 0
                }

    (_, final) <- runStateT parser initial
    V.freeze $ MV.slice 0 (parserEventCount final) (parserEvents final)
  where
    tokens = lexer input
