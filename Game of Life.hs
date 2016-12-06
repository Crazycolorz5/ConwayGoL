newtype Cell = Cell {getCell :: Bool}
instance Show Cell where
	show (Cell True) = "1"
	show _ = "0"

newtype Board = Board {getBoard :: [[Cell]]}
instance Show Board where
	show (Board cells) = show_rec $ transpose cells
		where
			show_rec::(Show a) => [[a]] -> String
			show_rec [] = ""
			show_rec matrix = (show $ head matrix)++('\n':(show_rec $ tail matrix))
			transpose::[[a]] -> [[a]] --prereq: matrix is rectangular
			transpose matrix 
				| null $ head matrix = []
				| otherwise 		 = (map head matrix):(transpose $ map tail matrix)

makeBoard::Int->Int->Board
makeBoard x y = Board $ take x $ repeat (take y $ repeat $ Cell False)

makeBoolBoard::[[Bool]]->Board
makeBoolBoard = Board . map (map Cell)

turnCell::Bool->Board->Int->Int->Board
turnCell val board x y = Board [[if xTrev==x&&yTrev==y then Cell val else ((getBoard board)!!xTrev)!!yTrev | yTrev <- (take (length . head $ getBoard board) $ [0..])] | xTrev <- (take (length $ getBoard board) [0..])]

setCell::Board->Int->Int->Board
setCell = turnCell True

unsetCell::Board->Int->Int->Board
unsetCell = turnCell False

toggleCell::Board->Int->Int->Board
toggleCell board x y = turnCell (not $ getCell $ ((getBoard board)!!x)!!y) board x y

updateCell_general::[Int]->[Int]->Board->Int->Int->Cell
updateCell_general stayLiveRules becomeLiveRules board x y = if (getCell $ tryGetBoard board x y) then (Cell (elem neighbors stayLiveRules)) else (Cell (elem neighbors becomeLiveRules))
	where
        getBoardState (a,b) = tryGetBoard board a b
        neighbors::Int
        neighbors = foldl (\acc->(\e->if(getCell e) then (acc+1) else (acc))) 0 (map getBoardState $ [(x-1, y-1),(x,y-1),(x+1, y-1), (x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)])

updateCell::Board->Int->Int->Cell
updateCell = updateCell_general [2,3] [3]

tryGetBoard::Board->Int->Int->Cell
tryGetBoard board x y
    | x < 0     = Cell False
    | y < 0     = Cell False
    | x >= length (getBoard board) = Cell False
    | y >= length (head (getBoard board)) = Cell False
    | otherwise = (getBoard board)!!x!!y

stepBoard::Board->Board
stepBoard board = Board [[updateCell board xTrev yTrev | yTrev <- (take (length . head $ getBoard board) $ [0..])] | xTrev <- (take (length $ getBoard board) [0..])]

getWorldline initialBoard = iterate stepBoard initialBoard-- initialBoard:(getWorldline $ stepBoard initialBoard)


testPattern = Board $ map (map (\x->(Cell $ x>0))) [[0,0,0,0,0],[0,1,1,0,0],[0,1,0,0,0],[0,0,0,1,0],[0,0,0,0,0]]
