import System.IO
import Data.Char

type Line = String
-- Counts the frequency of bits in each position, e.g.
-- [-2, 1, 3, 0]
-- Means we've seen 2 more 0s in the leftmost position, then 1 more 1, 3 more
-- 1s, then an even split in the rightmost position.
type Count = [Int]

data Criterion = Most | Least

-- variant of map that passes each element's index as a second argument to f
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

-- Assuming that all lines have the same length
initCount :: [Line] -> Count
initCount [] = []
initCount lines =
    let lineLength = length $ lines !! 0 in
    take lineLength $ repeat 0

linesToCounts :: [Line] -> [Count]
linesToCounts [] = []
linesToCounts lines =
    let bitToCountPos = \bit -> if bit == '0' then -1 else 1 in
    let lineToCount = \line -> map bitToCountPos line in
    map lineToCount lines

collapseCounts :: Count -> Count -> Count
collapseCounts c1 c2 =
    zipWith (+) c1 c2

total :: Line -> Int
total [] = 0
total line = reverseTotal $ reverse line

reverseTotal :: Line -> Int
reverseTotal [] = 0
reverseTotal (x : xs) = (digitToInt x) + 2 * (reverseTotal xs)

countToLine :: Count -> Line
countToLine [] = ""
countToLine count =
    -- Breaking ties: if even amount of 0s and 1s, then just make it a 1
    let oneOrZeros = map (\countPos -> if countPos < 0 then 0 else 1) count in
    map intToDigit oneOrZeros

flipBit :: Char -> Char
flipBit bit =
    case bit of
        '0' -> '1'
        '1' -> '0'
        _ -> bit

flipLine :: Line -> Line
flipLine [] = []
flipLine line = map flipBit line

solve :: [Line] -> Int
solve lines =
    let resultCount = foldr collapseCounts
                           (initCount lines)
                           (linesToCounts lines) in
    let resultLine = countToLine resultCount in
    let flipResult = flipLine resultLine in
    (total resultLine) * (total flipResult)

cullIndexByCriterion :: Int -> Criterion -> [Line] -> [Line]
cullIndexByCriterion index criterion lines =
    let matchingBit =
        case criterion of
            Most -> ??
            Least -> ??


cull :: Int -> Criterion -> [Line] -> Line
cull _ _ [] = []
cull _ _ [line] = line
cull index criterion lines =
    let culledPass = cullIndexByCriterion index criterion lines in
    cull (index + 1) criterion culledPass

solve2 :: [Line] -> Int
solve2 lines =
    let oxygen = total $ cull 0 Most lines in
    let co2 = total $ cull 0 Least lines in
    oxygen * co2

main :: IO()
main =
    openFile "input.txt" ReadMode
    >>= hGetContents
    >>= \strings -> return (lines strings)
    >>= \lines -> return (show $ solve lines)
    >>= putStrLn
