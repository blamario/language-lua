module Main where

import           Criterion.Main
import           Data.Functor.Compose (getCompose)
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text, unpack)
import qualified Data.Text.IO        as Text
import           System.Directory    (getDirectoryContents)
import           System.FilePath
import Text.Grampa (parseComplete, failureDescription)

import qualified Language.Lua.Annotated.Parser  as P
import qualified Language.Lua.Grammar  as G

main :: IO ()
main = do tests <- loadFiles "lua-5.3.1-tests" 
          defaultMain
            [ env (loadFiles "lua-5.3.1-tests") $ \files ->
                bench "Parsing Lua files from 5.3.1 test suite" $
                  nf (catMaybes . map (either (error . show) Just) . map (P.parseText P.chunk . snd)) files,
              env (loadFiles "lua-5.3.1-tests") $ \files ->
                bench "Grammaring Lua files from 5.3.1 test suite" $
                  nf (catMaybes . map (\(name, content)-> 
                                        either (error . ((name ++ ":\n") ++) . unpack . flip (failureDescription content) 3) Just
                                        $ getCompose $ G.chunk $ parseComplete G.luaGrammar content)) files,
             env (loadFiles "lua-5.3.1-tests") $ \files ->
                bgroup "Grammaring individual Lua files from 5.3.1 test suite" $ []
--                  map (\(path, content)-> bench path $ nf (either (error . show) id . getCompose . G.chunk . parseComplete G.luaGrammar) content) tests
            ]

loadFiles :: FilePath -> IO [(String, Text)]
loadFiles root = do
  let isLuaFile file = takeExtension file == ".lua"
      onlyLuaFiles = map (root </>) . filter isLuaFile
  luaFiles <- fmap onlyLuaFiles (getDirectoryContents root)
  mapM (\file-> ((,) file) <$> Text.readFile file) luaFiles
