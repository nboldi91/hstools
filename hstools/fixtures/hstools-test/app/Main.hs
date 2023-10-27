import SimpleTest

main = do
  file <- readFile "somefile.txt"
  putStrLn $ columnName $ read file 