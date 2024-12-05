import Data.List (sort, sortBy)

splitOnForList :: Eq a => a -> [a] -> [[a]]
splitOnForList _ [] = []
splitOnForList delim (x : xs)
    | x == delim = splitOnForList delim xs
    | otherwise = (x : takeWhile (/= delim) xs) : splitOnForList delim (dropWhile (/= delim) xs)

mySplit :: Char -> String -> [String]
mySplit delim str = words [if c == delim then ' ' else c | c <- str]


myFirstPartAlgorithm :: [(Int, Int)] -> [Int] -> Bool
myFirstPartAlgorithm rules order = validateOrder [] order
  where
    validateOrder :: [Int] -> [Int] -> Bool
    validateOrder _ [] = True
    validateOrder currentSet (x:xs) =
      let 
          prereqRules = [(a,b) | (a,b) <- rules, b == x]
          allGood = all (\(a,_b) -> (a `notElem` order) || (a `elem` currentSet)) prereqRules
      in
        allGood && validateOrder (currentSet ++ [x]) xs

buildComparisonRules :: [(Int, Int)] -> [((Int, Int), Ordering)]
buildComparisonRules = concatMap (\(a,b) -> [((a,b), LT), ((b,a), GT)])

comparator :: [((Int, Int), Ordering)] -> Int -> Int -> Ordering
comparator rules x y =
    case lookup (x,y) rules of
        Just ord -> ord
        Nothing  -> EQ

middlePage :: [Int] -> Int
middlePage lst = let n = length lst in lst !! (n `div` 2)



main :: IO()
main = do
    content <- readFile "input.txt"
    let rows = lines content
    let delimiter = "" :: String


    let [rules, order] = splitOnForList "" rows
    let splitOrder = map (splitOnForList ',') order
    let intOrder = map (map (\c -> read c :: Int)) splitOrder
    let splitRules = map (mySplit '|') rules
    let pairedRules = map (\[a, b] -> (read a :: Int, read b :: Int)) splitRules

    
    let results = [(o, myFirstPartAlgorithm pairedRules o) | o <- intOrder]
    
    let correctUpdates = [o | (o, True) <- results]
    let sumMiddle = sum (map middlePage correctUpdates)
    
    print sumMiddle

    let incorrectUpdates = [o | (o, False) <- results]

    let comparisonRules = buildComparisonRules pairedRules
    
    let fixedUpdates = map (sortBy (comparator comparisonRules)) incorrectUpdates
    let sumMiddleFixed = sum (map middlePage fixedUpdates)

    print sumMiddleFixed