-- Haskell Project -- April 30th, 2020 -------------------------------------------------------------
-- Sean Ryan Aguilar && Hugo Dominguez -------------------------------------------------------------

  --Reading user inputs and printing outputs -----------------------------------------
module Main where

	import Board
	import System.IO
	import System.Exit
	--import Data.Char
	
	main = do
		putStrLn "--Welcome to Omok Game--"
		let gameBoard = mkBoard 15
		putStrLn (boardToStr playerToChar gameBoard)
		play gameBoard
	
	-- We had problems, a stack overflow error, but we were able to fix the error
	-- by about the Lines 20 through 24, we were calling an empty board on itself
	play bd = do
		player <- readXY bd mkPlayer
		let boardP = mark (fst player) (snd player) bd mkPlayer
		putStrLn (boardToStr playerToChar boardP)
		checkWinner boardP mkPlayer
		player2<- readXY boardP mkOpponent
		let boardO = mark (fst player2) (snd player2) boardP mkOpponent
		putStrLn(boardToStr playerToChar boardO)
		checkWinner boardO mkOpponent
		play boardO

  --readXY:: [Int] -> Int -> IO(Int, Int)
	readXY bd p = do
		putStrLn ("Player "++ show p)
		putStr "Enter your x Coordinate: "
		x <- getCoord
		putStr "Enter your y Coordinate: "
		y <- getCoord
		--Checking if valid
		if isEmpty x y bd
		then return (x,  y)
		else do
			putStrLn "\n\nAlready Played, Try Again!\n\n"
			readXY bd p

	getCoord = do
			line <- getLine
			let parsed = reads line :: [(Int, String)] in
				if length parsed == 0
				then wrongCoord
				else let (x, _) = head parsed in
					if x > 0 
					then return x
					else if x == -1 -- Exits the program if -1 as the input value
					then exitProgram 
					else wrongCoord
				
			where
				wrongCoord = do
					putStrLn "Invalid Input!"
					getCoord
	
	--checkWinner :: [Int] -> Int -> void
	checkWinner bd p = do
		if(isWonBy bd p)
		then do
			putStrLn ("\tPlayer "++ show p ++" is the winner! :)")
			main
		else if(isDraw bd)
		then do 
			putStrLn "It's a draw !_!"
			main
		else return()

	exitProgram = do
		putStrLn "Goodbye, thanks for playing!"
		exitFailure