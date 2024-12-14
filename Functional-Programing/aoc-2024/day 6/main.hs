import qualified Data.Set as Set
import Data.ByteString (find)

partOne :: [String] -> (Int, Int) -> (Int, Int) -> Set.Set (Int, Int) -> Int -> Int -> Set.Set (Int, Int)
partOne grid (r, c) (dr, dc) seen rows cols
    | r + dr < 0 || r + dr >= rows || c + dc < 0 || c + dc >= cols = Set.insert (r, c) seen
    | grid !! (r + dr) !! (c + dc) == '#' = partOne grid (r, c) (dc, -dr) (Set.insert (r, c) seen) rows cols
    | otherwise = partOne grid (r + dr, c + dc) (dr, dc) (Set.insert (r, c) seen) rows cols

replaceObstacle :: [String] -> (Int, Int) -> [String]
replaceObstacle grid (r, c) = take r grid ++ [take c (grid !! r) ++ "#" ++ drop (c + 1) (grid !! r)] ++ drop (r + 1) grid

partTwo :: [String] -> (Int, Int) -> (Int, Int) -> Set.Set (Int, Int, Int, Int) -> Int -> Int -> Bool
partTwo grid (r, c) (dr, dc) seen rows cols
    | r + dr < 0 || r + dr >= rows || c + dc < 0 || c + dc >= cols = False
    | Set.member (r, c, dr, dc) seen = True
    | grid !! (r + dr) !! (c + dc) == '#' = partTwo grid (r, c) (dc, -dr) (Set.insert (r, c, dr, dc) seen) rows cols
    | otherwise = partTwo grid (r + dr, c + dc) (dr, dc) (Set.insert (r, c, dr, dc) seen) rows cols


main :: IO()
main = do
    content <- readFile "input.txt"
    let grid = lines content
    let rows = length grid
    let cols = length (head grid)

    -- we need to find the ^ in the grid
    let start = [(x, y) | x <- [0..rows - 1], y <- [0..cols - 1], grid !! x !! y == '^']
    
    let path = partOne grid (head start) (-1 , 0) Set.empty rows cols
    print $ length path

    let differentGrid = [replaceObstacle grid (x,y) | x <- [0..rows - 1], y <- [0..cols - 1], grid !! x !! y == '.']
    let stuckGrid = [grid | grid <- differentGrid, partTwo grid (head start) (-1, 0) Set.empty rows cols]
    print $ length stuckGrid
    

