{-# LANGUAGE DeriveDataTypeable #-}
module Chapterize where
import System.Console.CmdArgs
import Text.XML.HXT.Core
import Text.HandsomeSoup

data ShowFile = ShowFile {file :: Maybe FilePath}
              deriving (Show, Data, Typeable)

showFile = ShowFile
  {file = def &= typ "FILE" &= argPos 0}

main = do
    options <- cmdArgs showFile
    contents <- case options of
        ShowFile { file = Nothing } -> getContents
        ShowFile { file = Just f  } -> readFile f
    links <- runX $ parseHtml contents >>> css "h2" /> getText
    putStr contents
    mapM_ putStr links
