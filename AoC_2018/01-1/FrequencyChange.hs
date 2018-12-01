import System.IO

-- frequency as changes of +1, -2. +13. -10, et al
-- frequency starts at 0
-- need to find sum of a frequency listing

parseFrequencyItem :: String -> (Char, Int)
parseFrequencyItem [] = ('+', 0)
parseFrequencyItem (x:xs) = (x, read xs)

parseFrequency :: [String] -> [(Char, Int)]
parseFrequency (freqList) = map parseFrequencyItem freqList

addFrequency :: (Char, Int) -> Int -> Int
addFrequency freq total
  | (fst freq) == '+' = total + (snd freq)
  | (fst freq) == '-' = total - (snd freq)

sumFrequency :: [(Char, Int)] -> Int
sumFrequency freqList = foldr (\freq acc -> addFrequency freq acc) 0 (freqList)

findFinalFrequency = do
  ifile <- readFile "input.txt"
  let freqList = (lines ifile)
  let finalFreq = sumFrequency $ parseFrequency freqList
  writeOut (show finalFreq)
  
writeOut pdata = do
  writeFile "2018_01-1_rslt.txt" pdata
  
main :: IO ()
main = do findFinalFrequency