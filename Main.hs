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
    lastMove :: Float,
    ended :: Bool
  }

-- Drawing each game state
render :: SnakeGame -> Picture
render game
  |(ended game) =
      pictures [translate (-200) 0 $ scale 0.5 0.5 $ color white $ text "Game Over"
        ,translate (-200) (-80) $ scale 0.25 0.25 $ color white $ text "Press 'g' to restart"]
  |otherwise =
      pictures [snake, body, clock, food, xText, yText]
        where
          snake = uncurry translate (loc game) $ color white $ rectangleSolid 15 15
          body = pictures (renderSnake (prevLoc game))
          food = uncurry translate (foodLoc game) $ color orange $ rectangleSolid 15 15

          current = (time game)
          timeString = show current
          clock = translate 200 230 $ scale 0.15 0.15 $ color white $ text timeString

          -- for debugging
          xText = translate 200 200 $ scale 0.15 0.15 $ color white $  text (show (xHead game))
          yText = translate 200 180 $ scale 0.15 0.15 $ color white $ text (show (yHead game))

renderSnake [] = []
renderSnake (coord:t) =
  (uncurry translate coord $ color white $ rectangleSolid 15 15):renderSnake(t)

-- Initial state of the game
initialState :: SnakeGame
initialState = Game
  { loc = (0, 0),
    prevLoc = [],
    snakeLength = 1,
    foodLoc = (200, 1),
    time = 0,
    lastMove = 0, -- 0 up 1 down 2 right 3 left
    ended = False
  }

-- Control snake
handleKeys :: Event -> SnakeGame -> SnakeGame

xHead :: SnakeGame -> Float
xHead game = fst(loc game)

yHead :: SnakeGame -> Float
yHead game = snd(loc game)

step :: Float
step = 15

handleKeys (EventKey (Char 'w') _ _ _) game =
  game { loc = ((xHead game), up), lastMove = 0}
    where
      up = (yHead game) + step
      currentLoc = xHead game

handleKeys (EventKey (Char 's') _ _ _) game =
  game { loc = ((xHead game), down), lastMove = 1}
    where
      down = (yHead game) - step

handleKeys (EventKey (Char 'd') _ _ _) game =
  game { loc = (right, (yHead game)), lastMove = 2 }
    where
      right = (xHead game) + step

handleKeys (EventKey (Char 'a') _ _ _) game =
  game { loc = (left, (yHead game)), lastMove = 3 }
    where
      left = (xHead game) - step

handleKeys (EventKey (Char 'g') _ _ _) game =
  initialState

handleKeys _ game = game

tick seconds game = game { time = t', loc = (x,y), ended = go, prevLoc = snakeBody}
  where
    -- Old locations and velocities.
    t = time game

    -- New locations.
    t' = t + seconds

    -- Keeps moving in the same direction
    curx = xHead game
    cury = yHead game

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

    snakeBody = updateSnakebody (isTouchingFood game) (curx,cury) (prevLoc game)

    go = isGameOver game


    --check location
updateSnakebody False _ [] = []
updateSnakebody ateFood coord list
  | ateFood == False = (coord):removeLast(list)
  |otherwise = (coord):list

removeLast [h] = []
removeLast [] = []
removeLast (h:t) = h:removeLast(t)



-- update :: ViewPort -> Float -> SnakeGame -> SnakeGame
update _ = tick

isGameOver game
  | (xHead game) < -250 || (xHead game) > 250 = True
  | (yHead game) < -250 || (yHead game) > 250 = True
  | otherwise = False

isTouchingFood game =
  abs(fst(loc game) - fst(foodLoc game)) < 15 && abs(snd(loc game) - snd(foodLoc game)) < 15


main :: IO ()
main = play window background 15 initialState render handleKeys tick
