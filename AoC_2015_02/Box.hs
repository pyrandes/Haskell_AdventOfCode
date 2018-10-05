import Data.List
import Data.List.Split
import Data.Char
import System.IO

area d1 d2 = d1 * d2

slack :: Int -> Int -> Int -> Int
slack l w h =  min (min a1 a2) a3 
  where a1 = area l w
        a2 = area l h
        a3 = area w h

surfaceArea :: Int -> Int -> Int -> Int
surfaceArea l w h = 2 * a1 + 2 * a2 + 2 * a3
  where a1 = area l w
        a2 = area l h
        a3 = area w h

paperNeeded :: Int -> Int -> Int -> Int
paperNeeded l w h = (surfaceArea l w h) + (slack l w h)

paper :: [String] -> Int
paper b = paperNeeded l w h 
  where l = read (head (b))::Int
        w = read (head (tail (b)))::Int
        h = read (last (b))::Int

-- given: b = ("LxWxH")
--   (1) split b On "x" (mapPaper b)
splitX = splitOn "x"
xtoList b = splitX b

--   (2) convert items in b from string to integer
findPaperNeeded :: [Char] -> Int
findPaperNeeded bList = paper (xtoList bList)

findAllPaperNeeded :: [String] -> [Int]
findAllPaperNeeded b 
  | null b = []
  | otherwise = [findPaperNeeded (head b)] ++ findAllPaperNeeded (tail b)

readBoxesForPaper = do
  ifile <- readFile "2015_02_data.txt"
  let lBoxes = (lines ifile)
  let paperList = findAllPaperNeeded lBoxes
  let pneeded = sum paperList
  writeOut (show pneeded)
  
writeOut pdata = do
  writeFile "2015_02_rslt.txt" pdata