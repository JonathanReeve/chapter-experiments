{-# LANGUAGE DeriveDataTypeable #-}
module ShowFile where
import System.Console.CmdArgs

data ShowFile = ShowFile {file :: Maybe FilePath}
              deriving (Show, Data, Typeable)

showFile = ShowFile
  {file = def &= typ "FILE" &= argPos 0}

main = do
    options <- cmdArgs showFile
    contents <- case options of
        ShowFile { file = Nothing } -> getContents
        ShowFile { file = Just f  } -> readFile f
    putStr contents
