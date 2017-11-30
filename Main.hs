module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "snake" (500, 500) (0,0)

background :: Color
background = black

-- Location data type
type Location = (Float, Float)

-- Game data structure
data SnakeGame = Game
  { loc :: Location,
    prevLoc :: [Location],
    snakeLength :: Float,
    foodLoc :: Location,
    time :: Float,
    nextMove :: Float,
    ended :: Bool
  }

-- Drawing each game state
render :: SnakeGame -> Picture
render game
  | (ended game) =
      pictures [translate (-200) 0 $ scale 0.5 0.5 $ color white $ text "Game Over"
        ,translate (-200) (-80) $ scale 0.25 0.25 $ color white $ text "Press 'g' to restart"]
  | otherwise =
      pictures [snake, clock, food, xText, yText, length, body]
        where
          snake = uncurry translate (loc game) $ color white $ rectangleSolid 10 10
          food = uncurry translate (foodLoc game) $ color orange $ rectangleSolid 10 10

          body = pictures (renderBody (prevLoc game) (snakeLength game) )

          current = (time game)
          timeString = show current
          clock = translate 200 230 $ scale 0.15 0.15 $ color white $ text timeString

          xText = translate 200 200 $ scale 0.15 0.15 $ color white $  text (show (xHead game))
          yText = translate 200 180 $ scale 0.15 0.15 $ color white $ text (show (yHead game))
          length = translate 200 160 $ scale 0.15 0.15 $ color white $ text (show (snakeLength game))

renderBody :: [Location] -> Float -> [Picture]
renderBody (h:t) i
  | i > 0 = (uncurry translate h $ color white $ rectangleSolid 10 10) : renderBody t (i-1)

renderBody [] _ = []
renderBody _ 0 = []

-- Initial state of the game
initialState :: SnakeGame
initialState = Game
  { loc = (0, 0),
    prevLoc = [(0,0)],
    snakeLength = 3,
    foodLoc = (200, 0),
    time = 0,
    nextMove = 0, -- 0 up 1 down 2 right 3 left 4 none
    ended = False
  }

-- Control snake
handleKeys :: Event -> SnakeGame -> SnakeGame

xHead :: SnakeGame -> Float
xHead game = fst(loc game)

yHead :: SnakeGame -> Float
yHead game = snd(loc game)

step :: Float
step = 10


-- snake can still go back in on it self

handleKeys (EventKey (Char 'w') _ _ _) game =
  game { nextMove = 0}

handleKeys (EventKey (Char 's') _ _ _) game =
  game { nextMove = 1}

handleKeys (EventKey (Char 'd') _ _ _) game =
  game { nextMove = 2 }

handleKeys (EventKey (Char 'a') _ _ _) game =
  game { nextMove = 3 }

handleKeys (EventKey (Char 'g') _ _ _) game =
  initialState

handleKeys _ game = game

toMove :: Location -> Float -> Location
toMove loc move
  | move == 0 = (fst(loc), snd(loc) + step)
  | move == 1 = (fst(loc), snd(loc) - step)
  | move == 2 = (fst(loc) + step, snd(loc))
  | move == 3 = (fst(loc) - step, snd(loc))

eatFood :: Location -> Location -> Float -> Float
eatFood snake food length 
  | snake == food = length + 1
  | otherwise = length


tick seconds game = game { time = t', loc = newLoc, prevLoc = toAdd, snakeLength = newLength, ended = go}
  where
    -- Old locations and velocities.
    t = time game

    -- New locations.
    t' = t + seconds

    -- Keeps moving in the same direction

    toAdd = (loc game) : (prevLoc game)
    newLength = eatFood (loc game) (foodLoc game) (snakeLength game)
    newLoc = toMove (loc game) (nextMove game)
    go = isGameOver game



-- update :: ViewPort -> Float -> SnakeGame -> SnakeGame
update _ = tick

isGameOver game
  | (xHead game) < -250 || (xHead game) > 250 = True
  | (yHead game) < -250 || (yHead game) > 250 = True
  | otherwise = False

main :: IO ()
main = play window background 10 initialState render handleKeys tick
