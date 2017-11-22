module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "Pacman" (500, 500) (0,0)

background :: Color
background = black


-- Game data structure
data PacmanGame = Game
	{ pacmanLoc :: (Float, Float),
	  time :: Float,
	  lastMove :: Float
	} deriving Show


-- Drawing each game state
render :: PacmanGame -> Picture
render game = 
	pictures [pacman, clock]
	where
		pacman = uncurry translate (pacmanLoc game) $ color yellow $ arcSolid 45 315 15 
		current = (time game)
		timeString = show current
		clock = translate 200 200 $ scale 0.25 0.25 $ color white $ text timeString

-- Initial state of the game
initialState :: PacmanGame
initialState = Game
	{ pacmanLoc = (0, 0),
	  time = 0,
	  lastMove = 0 -- 0 up 1 down 2 right 3 left
	}

-- Control Pacman
handleKeys :: Event -> PacmanGame -> PacmanGame

-- probably should make a general function to return current x y coordinates
-- instead of using currentLoc

-- How far the pacman moves
step :: Float
step = 5

handleKeys (EventKey (Char 'w') _ _ _) game =
	game { pacmanLoc = (currentLoc, up), lastMove = 0}
		where
			up = snd(pacmanLoc game) + step
			currentLoc = fst(pacmanLoc game)

handleKeys (EventKey (Char 's') _ _ _) game =
	game { pacmanLoc = (currentLoc, down), lastMove = 1}
		where
			down = snd(pacmanLoc game) - step
			currentLoc = fst(pacmanLoc game)

handleKeys (EventKey (Char 'd') _ _ _) game =
	game { pacmanLoc = (right, currentLoc), lastMove = 2 }
		where
			right = fst(pacmanLoc game) + step
			currentLoc = snd(pacmanLoc game)

handleKeys (EventKey (Char 'a') _ _ _) game =
	game { pacmanLoc = (left, currentLoc), lastMove = 3 }
		where
			left = fst(pacmanLoc game) - step
			currentLoc = snd(pacmanLoc game)

handleKeys _ game = game

tick seconds game = game { time = seconds}


main :: IO ()
main = play window background 1 initialState render handleKeys tick


