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
    lastMove :: Float,
    ended :: Bool,
    gameMap :: [Integer]
  } deriving Show

-- animate pacman properly
orientation x
  | x == 0 = 270	
  | x == 1 = 90
  | x == 2 = 0
  | x == 3 = 180

pA x
  | even x = arcSolid 45 315 10
  | odd x = circleSolid 10

-- Drawing each game state
render :: PacmanGame -> Picture
render game =
  pictures [walls, pacman, clock]
  where
    pacman = uncurry translate (pacmanLoc game) $ rotate face $ color yellow $ pacAnimation
       where
       	face = orientation (lastMove game)
       	pacAnimation
       	  | even (round (time game)) = arcSolid 45 315 10
       	  | odd (round (time game)) = circleSolid 10

    current = (time game)
    timeString = show current
    clock = translate 200 200 $ scale 0.25 0.25 $ color white $ text timeString
    --  The bottom and top walls.
    walls = pictures (renderWalls (gameMap game) 0)

wallColor = greyN 0.5


wall x y =
  translate (x*25 - 125) (y*25 - 125) $
    color wallColor $
      rectangleWire 25 25

empty x y =
  translate (x*25) (y*25) $
    color black $
      rectangleSolid 25 25


renderWalls :: [Integer] -> Integer -> [Picture]

renderWalls [] _ = []

renderWalls (h:t) acc
  | h==1 = (wall (fromIntegral (acc `div` gridWidth)) (fromIntegral (acc `mod` gridWidth))) : renderWalls t (fromIntegral (acc+1))
  | otherwise = renderWalls t (fromIntegral (acc+1))

-- Initial state of the game
initialState :: PacmanGame
initialState = Game
  { pacmanLoc = (0, 0),
    time = 0,
    lastMove = 0, -- 0 up 1 down 2 right 3 left
    ended = False,
    gameMap = [1,1,1,1,1,1,1,1,1,1
      ,1,0,0,0,0,1,1,0,0,1
      ,1,1,1,1,0,1,1,0,1,1
      ,1,1,1,1,0,1,1,0,1,1
      ,1,1,1,1,0,1,1,0,1,1
      ,1,1,1,1,0,1,0,0,0,1
      ,1,1,1,1,0,1,1,1,0,1
      ,1,1,1,1,0,0,0,0,0,1
      ,1,1,1,1,1,1,1,1,1,1
      ,1,1,1,1,1,1,1,1,1,1
      ]
  }

gridWidth = 10

-- Control Pacman
handleKeys :: Event -> PacmanGame -> PacmanGame

-- probably should make a general function to return current x y coordinates
-- instead of using currentLoc

-- How far the pacman moves
step :: Float
step = 3

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

tick seconds game = game { time = t', pacmanLoc = (x,y)}
  where
    -- Old locations and velocities.
    t = time game

    -- New locations.
    t' = t + seconds

    -- Keeps moving from the start
    curx = fst(pacmanLoc game)
    cury = snd(pacmanLoc game)

    lst = (lastMove game)

    xCheck
      | lst == 2 = curx + step
      | lst == 3 = curx - step
      | otherwise = curx

    yCheck
      | lst == 0 = cury + step
      | lst == 1 = cury - step
      | otherwise = cury

    x = xCheck
    y = yCheck



-- update :: ViewPort -> Float -> PacmanGame -> PacmanGame
update _ = tick

main :: IO ()
main = play window background 30 initialState render handleKeys tick
