import Data.List (sort, words, delete)

stringToListOfInts :: String -> [Int]
stringToListOfInts str = map read (words str)

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (x:xs) = xs
removeAt n (x:xs) = x : removeAt (n-1) xs

isAdjacentSafe :: [Int] -> Bool
isAdjacentSafe [] = True
isAdjacentSafe [x] = True
isAdjacentSafe (x:y:xs) = abs (y - x) >= 1 && abs (y - x) <= 3 && isAdjacentSafe (y:xs)

isSafe :: [Int] -> Bool
isSafe xs = (xs == sort xs || xs == reverse (sort xs)) && isAdjacentSafe xs

addToSafe :: [Int] -> Int
addToSafe l = if isSafe l then 1 else 0

addToSafe2 :: [Int] -> Int
addToSafe2 l
    | isSafe l = 1
    | any (\x -> isSafe (removeAt x l)) [0 .. length l - 1] = 1 
    | otherwise = 0



main :: IO()
main = do
    contents <- readFile "input.txt"
    let linesOfFile = lines contents

    let listOfLevels = map stringToListOfInts linesOfFile
    
    let resultPart1 = sum (map addToSafe listOfLevels)
    let resultPart2 = sum (map addToSafe2 listOfLevels)

    print resultPart1
    print resultPart2

