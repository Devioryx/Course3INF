import Text.Regex.TDFA ((=~)) 

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str =
    let (first, rest) = break (== delim) str
    in first : case rest of
        [] -> []
        (_:xs) -> splitOn delim xs

processMatches :: [String] -> Bool -> Int -> Int
processMatches [] _ total = total
processMatches (x:xs) on total
    | x == "do()"   = processMatches xs True total
    | x == "don't()" = processMatches xs False total
    | on =
        let nums = map read (splitOn ',' (takeWhile (/= ')') (drop 4 x))) :: [Int]
            value = head nums * last nums
        in processMatches xs on (total + value)
    | otherwise = processMatches xs on total

main :: IO ()
main = do
    input <- readFile "input.txt" 
    let finds = input =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: [[String]]
    let extracted = map head finds
    let items = map (\x -> splitOn ',' (takeWhile (/= ')') (drop 4 x)) :: [String])  extracted
    let converted = map (\x -> (read (head x) :: Int, read (last x) :: Int)) items
    let total = sum (map (\(x,y) -> x * y) converted)
    print total

    input <- readFile "input.txt" 
    let finds = input =~ "do\\(\\)|don't\\(\\)|mul\\([0-9]{1,3},[0-9]{1,3}\\)" :: [[String]]
    let extracted = map head finds
    let total = processMatches extracted True 0
    print total







