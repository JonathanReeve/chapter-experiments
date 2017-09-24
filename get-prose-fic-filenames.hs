main :: IO ()
main = do
  contents <- readFile "pg-prosefic-ids.txt"
  let proseFicIDs = lines contents 
  print proseFicIDs
