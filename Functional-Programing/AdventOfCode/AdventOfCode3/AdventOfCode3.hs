import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isDigit)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str =
    let (first, rest) = break (== delim) str
    in first : case rest of
        [] -> []
        (_:xs) -> splitOn delim xs


main :: IO()
main = do
    input <- readFile "input.txt" 
    let content = lines input
    let allContent = concat content
    let substrings = filter (\x -> "mul(" `isPrefixOf` x && ")" `isSuffixOf` x) [take x (drop y allContent) | x <- [8..12], y <- [0..(length allContent - x)]]
    let values = map (\x -> drop 4 x) substrings
    let finalValues = map (\x -> take (length x - 1) x) values
    let numbersArray = map (\x -> splitOn ',' x) finalValues



    let filteredValues = filter (\x -> all isDigit (head x) && all isDigit (last x)) numbersArray
    let convertedValues = map (\x -> (read (head x) :: Int, read (last x) :: Int)) filteredValues
    let finalSum = sum (map (\(x,y) -> x * y) convertedValues)
    print finalSum