module Main where

import           Criterion.Main
import           Data.Functor.Compose (getCompose)
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import qualified Data.Text.IO        as Text
import           System.Directory    (getDirectoryContents)
import           System.FilePath
import Text.Grampa (parseComplete)

import qualified Language.Lua.Annotated.Parser  as P
import qualified Language.Lua.Grammar  as G

main :: IO ()
main = defaultMain
  [ env (loadFiles "lua-5.3.1-tests") $ \files ->
      bench "Parsing Lua files from 5.3.1 test suite" $
        nf (catMaybes . map (either (const Nothing) Just) . map (P.parseText P.chunk)) files,
    env (loadFiles "lua-5.3.1-tests") $ \files ->
      bench "Grammaring Lua files from 5.3.1 test suite" $
        nf (catMaybes . map (either (const Nothing) Just) . map (getCompose . G.chunk . parseComplete G.luaGrammar)) files
  ]

loadFiles :: FilePath -> IO [Text]
loadFiles root = do
  let isLuaFile file = takeExtension file == ".lua"
      onlyLuaFiles = map (root </>) . filter isLuaFile
  luaFiles <- fmap onlyLuaFiles (getDirectoryContents root)
  mapM Text.readFile luaFiles
