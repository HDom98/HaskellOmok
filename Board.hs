-- Haskell Project -- April 30th, 2020 -------------------------------------------------------------
-- Sean Ryan Aguilar && Hugo Dominguez -------------------------------------------------------------

module Board where
	--mkPlayer :: Int
	mkPlayer = 1
	
	--mkOpponent :: Int
	mkOpponent = 2
	
	--mkBoard :: Int -> [Int]
	mkBoard n = replicate (n*n) 0
	
	--size :: [Int] -> Int
	size bd = (floor . sqrt . fromIntegral . length) bd
	
	--row :: Int -> [Int] -> [Int]
	row y bd 
		|y < 1 || y >(size bd) = []
		|otherwise = splitRow y bd

	--splitRow :: Int -> [a] -> [a]
	splitRow 1 (h:t) = take (size (h:t)) (h:t) 
	splitRow y (h:t) = take (size (h:t)) (removedRow y (h:t))
		where removedRow y (h:t) = drop (size (h:t) * (y - 1)) (h:t)
	
	--column :: Int -> [Int] -> [Int]
	column x bd
		| x < 1 || x >(size bd) = []
		|otherwise = splitCol (size bd) (drop (x - 1) bd)
		
	--splitCol :: Int -> [Int] -> [Int]
	splitCol _ [] = []
	splitCol x list = ((head list) : (splitCol x (drop x list))) 
	
	
-- Checking places and placing stones ------------------------------------------------------------

	--mark :: Int -> Int -> [Int] -> Int -> [Int]
	mark x y bd p = left ++ [p] ++ right
			where (left, (_ : right)) = splitAt((((y - 1) * (size bd)) + (x - 1))) bd

	--isMarked :: Int -> Int -> [Int] -> Bool
	isMarked x y bd = not (isEmpty x y bd)   
	
	--isEmpty :: Int -> Int -> [Int] -> Bool
	isEmpty x y bd = ((marker x y bd) == 0)
	
	--isMarkedBy :: Int -> Int -> [Char] -> Char -> Bool
	isMarkedBy x y bd p = ((marker x y bd) == p)   

	--marker :: Int -> Int -> [Int] -> Int
	marker x y bd    
		| (((y - 1) * (size bd)) + (x - 1)) < 0 = 0
		| (((y - 1) * (size bd)) + (x - 1)) >= (length bd) = 0
		| otherwise = head right
		where right = drop ((((y - 1) * (size bd)) + (x - 1))) bd
	
-- Determining the outcome -----------------------------------------------------------------------
  
	--isWonBy :: [Int] -> Int -> Bool
	isWonBy bd p  = hasWinSeq bd p
	
	--hasWinSeq :: [Int] -> Int -> Bool
	hasWinSeq [] _  = False
	hasWinSeq(h:t) p 
	 | h == p && length t >= 4 = allP (take 4 t)|| allP (column 1 t) || hasWinSeq t p -- horizontal
-- | h == p && length t >= 1 =   allP (column 1 (h:t))|| hasWinSeq t p-- vertical
--	 | -- diagonal
	 | otherwise = hasWinSeq t p
	 where 
		allP [] = True 
		allP (h:t) = h == p && allP t
		
	--isDraw :: [Int] -> Bool
	isDraw bd = (isFull bd) && (not (isWonBy bd 2)) && (not (isWonBy bd 1))

	--isGameOver :: [Int] -> Bool
	isGameOver bd = (isDraw bd) || (isWonBy bd 2) || (isWonBy bd 1)
	
	--isFull :: [Int] -> Bool
	isFull [] = True
	isFull(h:t) = h/= 0 && isFull t
	
-- Converting to a string for printing ------------------------------------------------------------
  
	--boardToStr :: (Int -> Char) -> [Int] -> [Char]
	boardToStr playerToChar [] = []
	boardToStr playerToChar bd = allRowsToStrings playerToChar bd (size bd)

	--allRowsToStrings :: (Int -> Char) -> [Int] -> [Int] -> [Char]
	allRowsToStrings playerToChar bd 0       = []
	allRowsToStrings playerToChar bd currRow = (allRowsToStrings playerToChar bd (currRow - 1)) ++ (rowToString playerToChar (row currRow bd)) ++ ['\n'] 
	
	--rowToString:: (Int->Int) -> [Int] ->[Char]
	rowToString _ [] = []
	rowToString f (h:t) = [playerToChar h] ++ [' '] ++ (rowToString playerToChar t)
	
	--playerToChar :: Int -> Char
	playerToChar p =
		case  p of
		1 -> 'O'
		2 -> 'X'
		x -> '.'