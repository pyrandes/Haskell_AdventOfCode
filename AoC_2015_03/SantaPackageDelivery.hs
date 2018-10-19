import System.IO

startNd = (0, 0)

navigateHouses :: Char -> (Integer, Integer) -> (Integer, Integer)
navigateHouses dir node 
  | dir == '^' = (fst node + 1, snd node)
  | dir == '>' = (fst node, snd node + 1)
  | dir == 'v' = (fst node - 1, snd node)
  | dir == '<' = (fst node, snd node - 1)

performDelivery :: [Char] -> (Integer, Integer) -> [(Integer, Integer)]
performDelivery lDir node
  | null lDir = []
  | otherwise = [node] ++ performDelivery (tail lDir) (navigateHouses (head lDir) node)
  
collectUniqueHouses :: (Eq a) => [a] -> [a]
collectUniqueHouses (x:xs) = x : collectUniqueHouses (filter (/= x) xs)
collectUniqueHouses [] = []

readDirections = do
  ifile <- openFile "input.txt" ReadMode
  ofile <- openFile "output.txt" WriteMode
  contents <- hGetContents ifile
  let houseList = collectUniqueHouses (performDelivery contents startNd)
  let result = show (houseList)
  let resLen = show (length houseList)
  hPutStr ofile resLen
  hClose ifile
  hClose ofile