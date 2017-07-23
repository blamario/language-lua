{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Language.Lua.Grammar as Grammar
import qualified Language.Lua.Parser as Parser

import Control.Monad
import Data.Functor.Compose (getCompose)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO (getLine, readFile)
import Data.Typeable (Typeable)
import Options.Applicative
import Text.Grampa (parseComplete)
import Language.Lua.PrettyPrinter (LPretty(..), displayS, renderPretty)

import Prelude hiding (getLine, readFile)

data GrammarMode = ChunkMode | StatementMode | ExpressionMode
    deriving Show

data BackendMode = EarleyMode | GrampaMode
    deriving Show

data Opts = Opts
    { optsInteractive :: GrammarMode
    , optsBackend     :: BackendMode
    , optsIndex       :: Int
    , optsPretty      :: Bool
    , optsFile        :: Maybe FilePath
    } deriving Show

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> p)
        ( fullDesc
       <> progDesc "Parse a lua file, or parse interactively"
       <> header "Lua parser")

    p :: Parser Opts
    p = Opts
        <$> (mode <|> pure ExpressionMode)
        <*> (backend <|> pure GrampaMode)
        <*> (option auto (long "index" <> help "Index of ambiguous parse" <> showDefault <> value 0 <> metavar "INT"))
        <*> switch
            ( long "pretty"
              <> help "Pretty-print output")
        <*> optional (strArgument
            ( metavar "FILE"
              <> help "Lua file to parse"))

    mode :: Parser GrammarMode
    mode = ChunkMode      <$ switch (long "chunk")
       <|> StatementMode  <$ switch (long "statement")
       <|> ExpressionMode <$ switch (long "expression")

    backend :: Parser BackendMode
    backend = EarleyMode <$ switch (long "earley")
              <|> GrampaMode  <$ switch (long "grampa")

main' :: Opts -> IO ()
main' Opts{..} =
    case optsFile of
        Just file -> readFile file >>= go Parser.chunk Grammar.chunk file
        Nothing ->
            case optsInteractive of
                ChunkMode      -> forever $ getLine >>= go Parser.chunk Grammar.chunk "<stdin>"
                StatementMode  -> forever $ getLine >>= go Parser.stat  Grammar.stat  "<stdin>"
                ExpressionMode -> forever $ getLine >>= go Parser.exp   Grammar.exp   "<stdin>"
  where
    go :: (Show f, LPretty f, Typeable f) => 
          Parser.Parser f -> (forall x. Grammar.LuaGrammar x -> x f) -> String -> Text -> IO ()
    go p f filename contents = case optsBackend 
                               of EarleyMode -> either (error . show) succeed (Parser.parseNamedText p filename contents)
                                  GrampaMode -> case getCompose (f $ parseComplete Grammar.luaGrammar contents)
                                                of Right [x] -> succeed x
                                                   Right l -> putStrLn ("Ambiguous: " ++ show optsIndex ++ "/" ++ show (length l) ++ " parses") >> succeed (l !! optsIndex)
                                                   Left err -> error (show err)
    succeed x = if optsPretty
                then putStrLn $ displayS (renderPretty 1.0 80 (pprint x)) ""
                else print x
