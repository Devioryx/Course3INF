import Data.List (sort)


countOccurences :: (Eq a) => a -> [a] -> Int
countOccurences x list = length (filter (x ==) list)


main :: IO()
main = do
    contents <- readFile "input.txt"
    let linesOfFile = lines contents
    
    let firstPartition = map (read . head . words) linesOfFile

    let secondPartition = map (read . last . words) linesOfFile
    
    let firstPartitionSorted = sort firstPartition
    let secondPartitionSorted = sort secondPartition

    let pairs = zip firstPartitionSorted secondPartitionSorted
    let totalDistance = sum ( map (\(x,y) -> abs (x - y) ) pairs )

    print totalDistance

    let sumOfItems = sum (map (\(x, _) -> x * countOccurences x secondPartition) pairs)
    
    print sumOfItems
    
