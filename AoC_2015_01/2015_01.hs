import System.IO

pconv :: Char -> Int
pconv a =
  case a of
    '('  -> 1
    ')' -> (-1)
    _ -> 0

elevSum x = sum (map pconv x)

readFromFile = do
  dfile <- openFile "2015_01_data.txt" ReadMode
  ofile <- openFile "2015_01_result.txt" WriteMode
  contents <- hGetContents dfile
  let result = show (elevSum contents)
  hPutStr ofile result
  hClose dfile
  hClose ofile

-- b = map pconv readFromFile




