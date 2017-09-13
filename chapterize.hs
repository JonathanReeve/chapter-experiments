{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Chapterize where
import System.Console.CmdArgs
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Text hiding (map) 

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
    links <- runX $ parseHtml contents >>> css "h2" /> getText
    -- putStr contents
    -- TODO: strip out whitespace from links. Should be something like this. 
    -- newlinks <- map strip links
    -- print links
    mapM_ putStr links
