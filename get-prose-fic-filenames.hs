import Text.Regex.PCRE

main :: IO ()
main = do
  proseFicFile <- readFile "pg-prose-fic-ids.txt"
  let proseFicIDs = lines proseFicFile 
  let isProseFicID bookID = bookID `elem` proseFicIDs
  let isProseFic bookPath = isProseFicID $ getID bookPath
  htmlFilesFile <- readFile "html-files.txt"
  let htmlFilesPaths = lines htmlFilesFile
  let proseFicPaths = filter isProseFic htmlFilesPaths
  writeFile "pg-prose-fic-html-files.txt" $ unlines proseFicPaths

getID :: String -> String
getID str = do
  let results = str =~ "_([0-9]+)/" :: [[String]]
  concatMap last results
