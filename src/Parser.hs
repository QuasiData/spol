{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Parser where

import Lexer

import Control.Monad (forM_, unless, void, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.State (MonadState (put), StateT, get, gets, modify', runStateT)
import Data.IntSet qualified as IS
import Data.List (findIndex)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree qualified as TR
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

data Tree = Tree TreeKind [Tree] | Leaf Token | ErrorNode String
    deriving (Show, Eq)

data Event
    = Open {eventKind :: TreeKind, forwardParent :: Maybe Int}
    | Close
    | Advance
    | Error String
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

untilM_ :: (Monad m) => m Bool -> m () -> m ()
untilM_ cond action = go
  where
    go = do
        done <- cond
        unless done $ action >> go

assertM :: (Monad m) => m Bool -> m ()
assertM cond = do
    holds <- cond
    unless holds $
        error "assertion failed in parser, this is a bug"

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond action = do
    holds <- cond
    when holds action

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM cond action = do
    holds <- cond
    unless holds action

whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ cond action = do
    whenM cond $ do
        action
        whileM_ cond action

andM :: (Monad m) => [m Bool] -> m Bool
andM [] = pure True
andM (cond : conds) = do
    holds <- cond
    if holds
        then andM conds
        else pure False

orM :: (Monad m) => [m Bool] -> m Bool
orM [] = pure False
orM (cond : conds) = do
    holds <- cond
    if holds
        then pure True
        else orM conds

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

withNodeMarked :: TreeKind -> Parser s a -> Parser s (a, MarkClosed)
withNodeMarked tk parser = do
    mark <- open
    result <- parser
    closed <- close mark tk
    pure (result, closed)

withNode :: TreeKind -> Parser s a -> Parser s a
withNode tk parser = fst <$> withNodeMarked tk parser

nth :: Int -> Parser s TokenKind
nth i = do
    st <- get

    let fuel = parserFuel st
        tokens = parserTokens st
        pos = parserCursor st

    if fuel == 0
        then error "parser ran out of fuel, this is a bug"
        else do
            put st{parserFuel = fuel - 1}
            pure $ maybe EofToken tokenKind (tokens V.!? (pos + i))

at :: TokenKind -> Parser s Bool
at tk = (== tk) <$> nth 0

atAny :: [TokenKind] -> Parser s Bool
atAny kinds = (`elem` kinds) <$> nth 0

eof :: Parser s Bool
eof = at EofToken

advance :: Parser s ()
advance = do
    end <- eof
    if end
        then error "advance: should not call advance when eof, this is a bug"
        else do
            _ <- emit Advance
            modify' (\st -> st{parserFuel = 256, parserCursor = parserCursor st + 1})

eat :: TokenKind -> Parser s Bool
eat tk = do
    atToken <- at tk
    if atToken
        then do advance; pure True
        else pure False

expect :: TokenKind -> Parser s ()
expect tk = do
    ate <- eat tk
    if ate
        then pure ()
        else do
            tk' <- nth 0
            reportError $ "Expected " ++ show tk ++ " found " ++ show tk'

reportError :: String -> Parser s ()
reportError err = void $ emit (Error err)

advanceWithError :: String -> Parser s ()
advanceWithError err = withNode ErrorTree $ do
    reportError err
    advance

parseFile :: Parser s ()
parseFile = withNode FileTree $
    untilM_ eof $ do
        atFn <- at FnKeywordToken
        if atFn
            then parseFunc
            else advanceWithError "expected a function"

parseFunc :: Parser s ()
parseFunc = withNode FnTree $ do
    assertM (at FnKeywordToken)
    expect FnKeywordToken
    expect NameToken

    whenM (at LParenToken) parseParamList
    whenM (eat ArrowToken) parseTypeExpr
    whenM (at LCurlyToken) parseBlock

paramListRecovery :: [TokenKind]
paramListRecovery = [ArrowToken, LCurlyToken, FnKeywordToken]

parseParamList :: Parser s ()
parseParamList = withNode ParamListTree $ do
    assertM (at LParenToken)
    expect LParenToken
    loop
    expect RParenToken
  where
    loop = unlessM (orM [at RParenToken, eof]) $ do
        isName <- at NameToken
        if isName
            then do
                parseParam
                loop
            else do
                recover <- atAny paramListRecovery
                unless recover $ do
                    advanceWithError "expected parameter"
                    loop

parseParam :: Parser s ()
parseParam = withNode ParamTree $ do
    assertM (at NameToken)
    expect NameToken
    expect ColonToken
    parseTypeExpr
    unlessM (at RParenToken) $
        expect CommaToken

parseTypeExpr :: Parser s ()
parseTypeExpr = withNode TypeExprTree (expect NameToken)

stmtRecovery :: [TokenKind]
stmtRecovery = [FnKeywordToken]

exprFirst :: [TokenKind]
exprFirst = [IntToken, TrueKeywordToken, FalseKeywordToken, NameToken, LParenToken]

parseBlock :: Parser s ()
parseBlock = withNode BlockTree $ do
    assertM (at LCurlyToken)
    expect LCurlyToken
    loop
    expect RCurlyToken
  where
    loop = unlessM (orM [at RCurlyToken, eof]) $ do
        token <- nth 0
        case token of
            LetKeywordToken -> do parseStmtLet; loop
            ReturnKeywordToken -> do parseStmtReturn; loop
            _ -> do
                recover <- atAny exprFirst
                if recover
                    then do
                        parseStmtExpr
                        loop
                    else do
                        recover' <- atAny stmtRecovery
                        if recover'
                            then pure ()
                            else do
                                advanceWithError "expected statement"
                                loop

parseStmtLet :: Parser s ()
parseStmtLet = withNode StmtLetTree $ do
    assertM (at LetKeywordToken)
    expect LetKeywordToken
    expect NameToken
    expect EqToken
    parseExpr
    expect SemiToken

parseStmtReturn :: Parser s ()
parseStmtReturn = withNode StmtReturnTree $ do
    assertM (at ReturnKeywordToken)
    expect ReturnKeywordToken
    parseExpr
    expect SemiToken

parseStmtExpr :: Parser s ()
parseStmtExpr = withNode StmtExprTree $ do
    parseExpr
    expect SemiToken

rightBindsTighter :: TokenKind -> TokenKind -> Bool
rightBindsTighter left right =
    let f = \tk -> findIndex (elem tk) [[PlusToken, MinusToken], [StarToken, SlashToken]]
        rt = f right
        lt = f left
     in case (rt, lt) of
            (Just r, Just l) -> r > l
            (Just _, Nothing) -> True
            _ -> False

parseExpr :: Parser s ()
parseExpr = parseExprRec EofToken

parseExprRec :: TokenKind -> Parser s ()
parseExprRec left = do
    lhs <- parseExprDelimited
    forM_ lhs $ \mark -> do
        lhs' <- parseCalls mark
        loop lhs'
  where
    loop lhs = do
        right <- nth 0
        if rightBindsTighter left right
            then do
                mark <- openBefore lhs
                advance
                parseExprRec right
                lhs' <- close mark ExprBinaryTree
                loop lhs'
            else pure ()

parseExprDelimited :: Parser s (Maybe MarkClosed)
parseExprDelimited = do
    token <- nth 0
    case token of
        tk
            | tk `elem` [IntToken, TrueKeywordToken, FalseKeywordToken] -> node ExprLiteralTree advance
        NameToken -> node ExprNameTree advance
        LParenToken -> node ExprParenTree $ do
            expect LParenToken
            parseExpr
            expect RParenToken
        _ -> do
            assertM (not <$> atAny exprFirst)
            pure Nothing
  where
    node tk action = Just . snd <$> withNodeMarked tk action

parseCalls :: MarkClosed -> Parser s MarkClosed
parseCalls lhs = do
    isCall <- at LParenToken
    if isCall
        then do
            mark <- openBefore lhs
            parseArgList
            lhs' <- close mark ExprCallTree
            parseCalls lhs'
        else pure lhs

parseArgList :: Parser s ()
parseArgList = withNode ArgListTree $ do
    assertM (at LParenToken)
    expect LParenToken
    loop
    expect RParenToken
  where
    loop = unlessM (orM [at RParenToken, eof]) $ do
        recover <- atAny exprFirst
        if recover
            then do
                parseArg
                loop
            else pure ()

parseArg :: Parser s ()
parseArg = withNode ArgTree $ do
    parseExpr
    whenM (not <$> at RParenToken) (expect CommaToken)

data BuildFrame = BuildFrame TreeKind [Tree]

data BuildState = BuildState
    { buildFrames :: [BuildFrame]
    , buildTokenIndex :: !Int
    , buildSkipped :: !IS.IntSet
    , buildRoot :: Maybe Tree
    }

buildTree :: Vector Token -> Vector Event -> Tree
buildTree tokens events = finished $ V.ifoldl' step initial events
  where
    initial = BuildState [] 0 IS.empty Nothing

    finished st = case st of
        BuildState [] _ _ (Just tree) -> tree
        BuildState [] _ _ Nothing -> error "buildTree: no root node, this is a bug"
        _ -> error "buildTree: unclosed nodes, this is a bug"

    step st i event
        | IS.member i (buildSkipped st) = st
        | otherwise = case event of
            Open{} -> openNode st i
            Advance -> advanceNode st
            Close -> closeNode st
            Error err -> errorNode st err

    openNode st i =
        let (tks, skipped) = followForwardPointers i
            frames =
                foldl'
                    (\stack tk -> BuildFrame tk [] : stack)
                    (buildFrames st)
                    tks
         in st{buildFrames = frames, buildSkipped = IS.union skipped (buildSkipped st)}

    advanceNode st =
        case tokens V.!? (buildTokenIndex st) of
            Just token ->
                case buildFrames st of
                    (BuildFrame tk children : rest) ->
                        st
                            { buildFrames = BuildFrame tk (Leaf token : children) : rest
                            , buildTokenIndex = buildTokenIndex st + 1
                            }
                    [] -> error "buildTree: not open tree, this is a bug"
            Nothing -> error "buildTree: token index out of range, this is a bug"

    errorNode st err =
        case buildFrames st of
            (BuildFrame tk children : rest) ->
                st{buildFrames = BuildFrame tk (ErrorNode err : children) : rest}
            [] -> error "buildTree: not open tree, this is a bug"

    closeNode st =
        case buildFrames st of
            [] -> error "buildTree: no nodes to close, this is a bug"
            BuildFrame tk children : rest ->
                let tree = Tree tk (reverse children)
                 in case rest of
                        BuildFrame parent siblings : ancestors ->
                            st{buildFrames = BuildFrame parent (tree : siblings) : ancestors}
                        [] -> case buildRoot st of
                            Nothing -> st{buildFrames = [], buildRoot = Just tree}
                            Just _ -> error "buildTree: multiple root nodes, this is a bug"

    followForwardPointers start = go start [] IS.empty
      where
        go i tks skipped =
            case events V.!? i of
                Just (Open tk parent) -> case parent of
                    Nothing -> (tk : tks, skipped)
                    Just ptr
                        | i < ptr -> go ptr (tk : tks) (IS.insert ptr skipped)
                        | otherwise -> error "buildTree: forwardParent points back, this is a bug"
                _ -> error "buildTree: not an open event, this is a bug"

prettyPrintTree :: Tree -> String
prettyPrintTree = TR.drawTree . convert
  where
    convert (Leaf token) = TR.Node (show token) []
    convert (Tree tk children) = TR.Node (show tk) (map convert children)
    convert (ErrorNode err) = TR.Node err []

parseFileTest :: String -> IO ()
parseFileTest path = do
    input <- readFile path
    let tree = parseTest parseFile (T.pack input)
    putStr $ prettyPrintTree tree

parseTest :: (forall s. Parser s a) -> Text -> Tree
parseTest parser input = buildTree tokens $ runST $ do
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
    tokens = V.filter (not . isTrivia . tokenKind) (lexer input)
