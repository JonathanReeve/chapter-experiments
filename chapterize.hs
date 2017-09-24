{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Chapterize where
import System.Console.CmdArgs
import Text.XML.HXT.Core
import Text.HandsomeSoup
--import Data.Text hiding (map, filter)

data CLI = CLI {file :: Maybe FilePath}
              deriving (Show, Data, Typeable)

cli :: CLI
cli = CLI
  {file = def &= typ "FILE" &= argPos 0}
  &= verbosity
  &= program "chapterize"
  &= help "Break a text into chapters."
  &= summary "Chapterize v2.0.1, (C) Jonathan Reeve"

main :: IO ()
main = do
    options <- cmdArgs cli
    contents <- case options of
        CLI { file = Nothing } -> getContents
        CLI { file = Just f  } -> readFile f
    --h1 <- runX $ parseHtml contents >>> css "h1" /> getText
    --h2 <- runX $ parseHtml contents >>> css "h2" /> getText
    anames <- runX $ parseHtml contents >>> css "a" >>> getAttrValue "name"
    --print $ file options 
    --print h1
    --print h2
    print $ filter (/= "") anames
