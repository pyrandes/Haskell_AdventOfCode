import System.IO

-- frequency as changes of +1, -2. +13. -10, et al
-- frequency starts at 0
-- need to find incremental sums of a frequency listing and build a list
-- checking to see if we had previoulsy added the sum to the list

-- SECTION frequency parsing and accumulation ----
parseFrequencyItem :: String -> (Char, Int)
parseFrequencyItem [] = ('+', 0)
parseFrequencyItem (x:xs) = (x, read xs)

parseFrequency :: [String] -> [(Char, Int)]
parseFrequency (freqList) = map parseFrequencyItem freqList

addFrequency :: Int -> (Char, Int) -> Int
addFrequency total freq
  | (fst freq) == '+' = total + (snd freq)
  | (fst freq) == '-' = total - (snd freq)
--------------------------------------------------


-- (1) utilize (elem freq calcdFreqList) to find match
-- (2) build calcdFreqList: 
buildCalcdFreqList :: (Char, Int) -> [Int] -> [Int]
buildCalcdFreqList freqCharge calcdFreqList = (addFrequency (head calcdFreqList) freqCharge) : calcdFreqList

buildFullCalcFreqList :: [(Char, Int)] -> [Int] -> [Int]
buildFullCalcFreqList chargeList freqList 
  | null chargeList = freqList
  | null freqList = buildFullCalcFreqList (chargeList) [0]
  | otherwise = buildFullCalcFreqList (tail chargeList) (buildCalcdFreqList (head chargeList) freqList)

findFirstDupFreq :: [(Char, Int)] -> [Int] -> Int
findFirstDupFreq freqChargeList freqList
  | null freqList = findFirstDupFreq freqChargeList [0]
  | elem (addFrequency (head freqList) (head freqChargeList)) freqList = (addFrequency (head freqList) (head freqChargeList))
  | otherwise = findFirstDupFreq ((tail freqChargeList) ++ [(head freqChargeList)]) (buildCalcdFreqList (head freqChargeList) freqList)

findFinalFrequency = do
  ifile <- readFile "input.txt"
  let freqChargeList = (lines ifile)
  let firstFreqDupe = findFirstDupFreq (parseFrequency freqChargeList) []
  writeOut (show firstFreqDupe)
  
writeOut pdata = do
  writeFile "2018_01-2_rslt.txt" pdata
  
main :: IO ()
main = do findFinalFrequency