import Text.Regex.PCRE

getID :: String -> String
getID str = do
  let results = str =~ "([0-9]+).xml" :: [[String]]
  unlines $ map last results

main :: IO ()
main = interact getID
