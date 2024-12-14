main :: IO ()
main = do
    contents <- readFile "file.txt"
    let grid = lines contents
    let numRows = length grid
    let numCols = length (head grid)
    let positions = [(row, col) | row <- [0..(numRows -1)], col <- [0..(numCols -1)], grid !! row !! col == 'X']
    let directions = [(dr, dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], dr /= 0 || dc /= 0]
    let items = [(row, col) | (row, col) <- positions, (dr, dc) <- directions, 
                             let indices = [(row + wordLength * dr, col + wordLength * dc) | wordLength <- [0..3]],
                             all (\(i, j) -> i >= 0 && i < numRows && j >= 0 && j < numCols) indices,
                             let chars = [grid !! (row + wordLength * dr) !! (col + wordLength * dc) | wordLength <- [0..3]],
                             chars == "XMAS"]
    print $ length items


    let validPatterns = ["MMSS", "MSSM", "SSMM", "SMMS"]
    let positions = [ (row, col) | row <- [1 .. numRows - 2],
                               col <- [1 .. numCols - 2],
                               (grid !! row) !! col == 'A' ]
    let count = length [ () | (row, col) <- positions,
                              let corners = [ (grid !! (row - 1)) !! (col - 1),
                                              (grid !! (row - 1)) !! (col + 1),
                                              (grid !! (row + 1)) !! (col + 1),
                                              (grid !! (row + 1)) !! (col - 1) ],
                              corners `elem` validPatterns ]
    print count