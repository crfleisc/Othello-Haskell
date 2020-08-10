

import Debug.Trace 
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
--import System.Random
import Data.List ((\\))
import System.Environment
import System.IO.Unsafe
import System.Exit
import Data.Either
import OthelloTools



{-  

	-- STRATEGY FUNCTIONS -- 
	
	
	choose best move based on multiple criterion (create heirarchy of component strategies)
		choose best move based on a single criterion (max/min in list of possible moves)

	other strategies	
		identify if a move touches a wall
		identify if a move touches a corner
	
	make list of number of moves left for opponent after all possible moves (choose min)
		count number of moves left to opponent after a single move
	
	make list of pieces flipped for all moves (choose max)
		count pieces flipped for a move
	
	find all valid moves
		find a valid move    
	
	check all positions on board (diagonals and columns)
		check a position on board (rows)
	
	flip a row of pieces
		flip a piece				
		
		
	-- COMPONENTS OF A TURN --
	1. get board
	2. choose a move
	3. update board
	
	
	-- GAME --
	playGame = do
		let blackTurn = blackStrat currentBoard 
		let whiteTurn = whiteStrat currentBoard
		if doublePass blackturn whiteTurn == true then printResults
		else playGame
-}

---Main-------------------------------------------------------------

main = main' (unsafePerformIO getArgs)
    
{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
    
main'           :: [String] -> IO()
main' args = do
	-- CHECK FOR ARGUMENTS CORRESPONDINGTO A STRATEGY CHOICE --

	
    -- STRATEGY NAMES --
	-- not currently used, would like to eventually correlate selection with position in list
	-- ie. strategy1 = stratOptions[0]
    let stratOptions = ["strategy1", "strategy2", "strategy3", "strategy4"]
	
	-- PRINT STRATEGIES --
    let printStrats = putStrLn "\n\n1. STRATEGY 1\n2. STRATEGY 2\n3. STRATEGY 3\n4. STRATEGY 4"

    --checkStratChoice :: (Num) => x -> IO()    
    let checkStratChoice x 
		| x == "1" =  putStrLn "picked 1" 
		| x == "2" =  putStrLn "picked 2" 
		| x == "3" =  putStrLn "picked 3" 
		| x == "4" =  putStrLn "picked 4" 
		| otherwise = printStrats && error "INVALID SELECTION" 
		
	-- START ACTUAL EXECUTION OF CODE
	do 
		printStrats
		putStr "\nEnter strategy for black: "
		blackSelection <- getLine
		checkStratChoice blackSelection					 
    
	printStrats
	putStr "\nEnter strategy for white: "
	whiteSelection <- getLine
	checkStratChoice whiteSelection

    putStrLn "Gameplay begins here"
	----------------------------------------
	
	
    let boardCurrent = (theBoard initBoard)
    gameProgress boardCurrent 0 0
    
gameProgress boardCurrent x y = do
	--if color == white 
	--then color = black 
	--else color = white
	let boardNext = replace2 boardCurrent (x,y) B
    print boardNext
    if x<7
    then gameProgress boardNext (inc x) y
    else
	if y<7 
	then gameProgress boardNext 0 (inc y)
    	else
	    putStrLn "Finished printing the board"
      

	
    
--    putStrLn "\nThe initial board with reallyStupidStrategy having played one move (clearly illegal!):"
--    let mv = reallyStupidStrategy (initBoard) B   --B is the turn of black
--       in case mv of
--          Nothing   -> putStrLn "Black passed."   --nothing if the player passed
--          (Just pt) -> putBoard $ replace2 (theBoard initBoard) pt B




---Strategies-------------------------------------------------------
{- | This is the type for all player functions.  A player strategy function takes a 
     board and returns a point (Int,Int) that it chooses -- this should be a legal move.  
     If the player passes, the funciton should return Nothing.
-}           
type Chooser = GameState -> Cell -> Maybe (Int,Int)  --returns a move maybe(int,int) ,,, i believe

-- | This strategy lives up to it's name: it always chooses to play at cell (0,0).
reallyStupidStrategy  :: Chooser
reallyStupidStrategy b c = Just (0,0)

-- | another stupid strategy
alsoStupidStrategy :: Chooser
alsoStupidStrategy b c = Just (7,7)


---Board rotations-------------------------------------------------------------

-- | Rotate a board 90 degrees clockwise.
rotateClock     :: [[a]] -> [[a]]
rotateClock       [ [a0, a1, a2, a3, a4, a5, a6, a7],
                    [b0, b1, b2, b3, b4, b5, b6, b7],
                    [c0, c1, c2, c3, c4, c5, c6, c7],
                    [d0, d1, d2, d3, d4, d5, d6, d7],
                    [e0, e1, e2, e3, e4, e5, e6, e7],
                    [f0, f1, f2, f3, f4, f5, f6, f7],
                    [g0, g1, g2, g3, g4, g5, g6, g7],
                    [h0, h1, h2, h3, h4, h5, h6, h7] ] =
                  [ [h0, g0, f0, e0, d0, c0, b0, a0],
                    [h1, g1, f1, e1, d1, c1, b1, a1],
                    [h2, g2, f2, e2, d2, c2, b2, a2],
                    [h3, g3, f3, e3, d3, c3, b3, a3],
                    [h4, g4, f4, e4, d4, c4, b4, a4],
                    [h5, g5, f5, e5, d5, c5, b5, a5],
                    [h6, g6, f6, e6, d6, c6, b6, a6],
                    [h7, g7, f7, e7, d7, c7, b7, a7] ]

-- | Rotate a board 90 degrees counter clockwise.
rotateCounter     [ [h0, g0, f0, e0, d0, c0, b0, a0],
                    [h1, g1, f1, e1, d1, c1, b1, a1],
                    [h2, g2, f2, e2, d2, c2, b2, a2],
                    [h3, g3, f3, e3, d3, c3, b3, a3],
                    [h4, g4, f4, e4, d4, c4, b4, a4],
                    [h5, g5, f5, e5, d5, c5, b5, a5],
                    [h6, g6, f6, e6, d6, c6, b6, a6],
                    [h7, g7, f7, e7, d7, c7, b7, a7] ] =
                  [ [a0, a1, a2, a3, a4, a5, a6, a7],
                    [b0, b1, b2, b3, b4, b5, b6, b7],
                    [c0, c1, c2, c3, c4, c5, c6, c7],
                    [d0, d1, d2, d3, d4, d5, d6, d7],
                    [e0, e1, e2, e3, e4, e5, e6, e7],
                    [f0, f1, f2, f3, f4, f5, f6, f7],
                    [g0, g1, g2, g3, g4, g5, g6, g7],
                    [h0, h1, h2, h3, h4, h5, h6, h7] ]



-- | These functions return a rotated point the same as the board rotations above.
type PointRotation = (Int,Int) -> (Int,Int)

-- | rotate 90 degrees clockwise.
rotatePt             :: PointRotation
rotatePt             (x,y) = (7-y, x)   

-- | rotate 90 degrees counterclockwise

rotatePtC            :: PointRotation
rotatePtC	     (x,y) = (x, 7-x)
               
---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                    in  (if null zs then (if null ys then [] else init ys) else ys) ++ [elem] ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

--parse string to see what's in (0,0)cell
--firstCell       :: [[a]] -> (Int,Int) -> a
--firstCell xs (x,y) = (x,y)

--a "variable" for counting purposes
g = 0

--incrementing function
inc :: Int -> Int
inc n = n+1
