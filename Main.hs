module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

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
    ended :: Bool,
    control :: Bool, -- True computer False human
    menu :: Bool,
    difficulty :: Int,-- 3 being eazy and 1 being hard
    frameTracker :: Int,
    aiPath :: [Float]
  }

step :: Float
step = 10

-- Drawing each game state
render :: SnakeGame -> Picture
render game
  | (ended game) =
      pictures [translate (-200) 0 $ scale 0.5 0.5 $ color white $ text "Game Over"
        ,translate (-200) (-80) $ scale 0.25 0.25 $ color white $ text "Press 'r' to restart"]
  | (menu game) =
      pictures [translate (-150) 80 $ scale 0.5 0.5 $ color white $ text "Snake"
        ,translate (-150) (-30) $ scale 0.15 0.15 $ color compColor $ text "(C)omputer Controlled"
        ,translate (-150) (-50) $ scale 0.15 0.15 $ color humanColor $ text "H(u)man Controlled"
        ,translate (-150) (-80) $ scale 0.10 0.10 $ color (modeColor game 3) $ text "(E)asy"
        ,translate (-150) (-100) $ scale 0.1 0.10 $ color (modeColor game 2) $ text "(M)edium"
        ,translate (-150) (-120) $ scale 0.1 0.10 $ color (modeColor game 1) $ text "(H)ard"
        ,translate (-100) (-150) $ scale 0.2 0.2 $ color white $ text "Start (G)ame"]
  | otherwise =
      pictures [snake, clock, food, xText, yText, length, body, aiDir]
        where
          snake = uncurry translate (loc game) $ color white $ rectangleSolid 10 10
          food = uncurry translate (foodLoc game) $ color yellow $ rectangleSolid 10 10

          body = pictures (renderBody (prevLoc game) (snakeLength game) )

          current = (time game)
          timeString = show current
          clock = translate 200 230 $ scale 0.15 0.15 $ color white $ text timeString

          -- for debuggin
          xText = translate 200 200 $ scale 0.15 0.15 $ color white $  text (show (xHead game))
          yText = translate 200 180 $ scale 0.15 0.15 $ color white $ text (show (yHead game))
          length = translate 200 160 $ scale 0.15 0.15 $ color white $ text (show (snakeLength game))

          dir
            |(aiPath game) == [] = 5
            |otherwise = (head (aiPath game))

          aiDir = translate 200 220 $ scale 0.15 0.15 $ color white $ text (show dir)

          humanColor =
            if (control game)
              then white
              else yellow
          compColor =
            if (control game)
              then yellow
              else white

modeColor game stepConstant
  | (difficulty game) == stepConstant = yellow
  | otherwise = white

renderBody :: [Location] -> Float -> [Picture]
renderBody (h:t) i
  | i > 0 = (uncurry translate h $ color white $ rectangleSolid 10 10) : renderBody t (i-1)

renderBody [] _ = []
renderBody _ 0 = []

-- Initial state of the game
initialState :: SnakeGame
initialState = Game
  { loc = (0, 0),
    prevLoc = [],
    snakeLength = 3,
    foodLoc = (0, 200),
    time = 0,
    nextMove = 0, -- 0 up 1 down 2 right 3 left 4 none
    ended = False,
    control = False,
    menu = True,
    difficulty = 2,
    frameTracker = 0,
    aiPath = []
  }


handleKeys :: Event -> SnakeGame -> SnakeGame

xHead :: SnakeGame -> Float
xHead game = fst(loc game)

yHead :: SnakeGame -> Float
yHead game = snd(loc game)


handleKeys (EventKey (Char 'w') _ _ _) game =
  game { nextMove = 0}

handleKeys (EventKey (Char 's') _ _ _) game =
  game { nextMove = 1}

handleKeys (EventKey (Char 'd') _ _ _) game =
  game { nextMove = 2 }

handleKeys (EventKey (Char 'a') _ _ _) game =
  game { nextMove = 3 }

handleKeys (EventKey (Char 'g') _ _ _) game
  | (menu game) = game {menu = False}
  | otherwise = game

handleKeys (EventKey (Char 'r') _ _ _) game
  | (ended game) = initialState
  | otherwise = game


-- Game control options
handleKeys (EventKey (Char 'c') _ _ _) game =
  game {control = True}

handleKeys (EventKey (Char 'u') _ _ _) game =
  game {control = False}



handleKeys (EventKey (Char 'e') _ _ _) game
  | (menu game) = game {difficulty = 3}
  | otherwise = game

handleKeys (EventKey (Char 'm') _ _ _) game
  | (menu game) = game {difficulty = 2}
  | otherwise = game

handleKeys (EventKey (Char 'h') _ _ _) game
  | (menu game) = game {difficulty = 1}
  | otherwise = game

handleKeys _ game = game

toMove :: Location -> Float -> Location
toMove loc move
  | move == 0 = (fst(loc), snd(loc) + step)
  | move == 1 = (fst(loc), snd(loc) - step)
  | move == 2 = (fst(loc) + step, snd(loc))
  | move == 3 = (fst(loc) - step, snd(loc))
  -- handle move from ai path where it cannot find a path
  | move == 5 = (fst(loc) - step, snd(loc))

closestToFood :: Location -> Location -> Float
closestToFood loc food
  | (fst(food) == fst(loc) && snd(food) > snd(loc)) = 0
  | (fst(food) == fst(loc) && snd(food) < snd(loc)) = 1
  | (snd(food) == snd(loc) && fst(food) > fst(loc)) = 2
  | (snd(food) == snd(loc) && fst(food) < fst(loc)) = 3
  | (fst(food) > fst(loc) && snd(food) > snd(loc)) = 0
  | (fst(food) > fst(loc) && snd(food) < snd(loc)) = 1
  | (fst(food) < fst(loc) && snd(food) > snd(loc)) = 0
  | (fst(food) < fst(loc) && snd(food) < snd(loc)) = 1

eatFood :: Location -> Location -> Float -> Float
eatFood snake food length
  | snake == food = length + 1
  | otherwise = length

compPath :: Location -> Location -> [Location] -> Float -> [Float]
compPath loc food prevLoc len
  | food == loc = []
  | not(isSelfColliding len move_0 prevLoc 0) && (path_0 == [] || (head(path_0) /= 5))
        = ((dirs!!0) : path_0)
  | not(isSelfColliding len move_1 prevLoc 0) && (path_1 == [] || (head(path_1) /= 5))
        = ((dirs!!1) : path_1)
  | not(isSelfColliding len move_2 prevLoc 0) && (path_2 == [] || (head(path_2) /= 5))
        = ((dirs!!2) : path_2)
  | not(isSelfColliding len move_3 prevLoc 0) && (path_3 == [] || (head(path_3) /= 5))
        = ((dirs!!3) : path_3)
  | otherwise = [5] -- no solutions will crash anyways
    where
      dirs = getShortestDirections loc food

      move_0 = toMove loc (dirs!!0)
      path_0 = (compPath move_0 food (loc:prevLoc) len)

      move_1 = toMove loc (dirs!!1)
      path_1 = (compPath move_1 food (loc:prevLoc) len)

      move_2 = toMove loc (dirs!!2)
      path_2 = (compPath move_2 food (loc:prevLoc) len)
      --
      move_3 = toMove loc (dirs!!3)
      path_3 = (compPath move_3 food (loc:prevLoc) len)

getShortestDirections (x1,y1) (x2,y2) = result
  where
    dx = x2 - x1
    dy = y2 - y1

    hor_dir
      | dx > 0 = [2,3]
      | dx < 0 = [3,2]
      | otherwise = [2,3]

    ver_dir
      | dy > 0 = [0,1]
      | dy < 0 = [1,0]
      | otherwise = [0,1]

    adx = abs dx
    ady = abs dy
    result
      | adx == 0 = [head ver_dir, head hor_dir, last hor_dir, last ver_dir]
      | ady == 0 = [head hor_dir, head ver_dir, last ver_dir, last hor_dir]
      | adx < ady = [head ver_dir, head hor_dir, last hor_dir, last ver_dir]
      | adx > ady = [head hor_dir, head ver_dir, last ver_dir, last hor_dir]
      | otherwise = [head ver_dir, head hor_dir, last hor_dir, last ver_dir]
tick seconds game
  | (ended game) || (menu game) = game
  | (frameTracker game) == 0 =
      game { time = t',
            loc = newLoc,
            prevLoc = toAdd,
            snakeLength = newLength,
            ended = go,
            foodLoc = newFoodLoc,
            aiPath = newAiPath,
            frameTracker = ft'}
  | otherwise = game {time = t', frameTracker = ft'}
        where
          -- Old locations and velocities.
          t = time game

          -- New locations.
          t' = t + seconds

          ft = frameTracker game

          ft' = mod (ft + 1) (difficulty game)

          -- Keeps moving in the same direction

          toAdd = (loc game) : (prevLoc game)
          newLength = eatFood (loc game) (foodLoc game) (snakeLength game)

          newFoodLoc =
            if (loc game) == (foodLoc game)
              then generateRandomCoordinates (round (t * 100000))
              else (foodLoc game)

          curAiPath
            | (aiPath game) == [] && (control game) =
              compPath (loc game) newFoodLoc (prevLoc game) newLength
            | otherwise = (aiPath game)

          newLoc
            | (control game) = toMove (loc game) (head curAiPath)
            | otherwise = toMove (loc game) (nextMove game)

          newAiPath
            | (control game) = tail (curAiPath)
            | otherwise = curAiPath

          go = isGameOver game



generateRandomCoordinates t = do
  let s1 = mkStdGen t
  let (i1, s2) = randomR (-25, 25 :: Int) s1
  let (i2,  _) = randomR (-25, 25 :: Int) s2
  ((fromIntegral i1 * 10),(fromIntegral i2 * 10))

-- update :: ViewPort -> Float -> SnakeGame -> SnakeGame
update _ = tick

isGameOver game
  | (xHead game) < -250 || (xHead game) > 250 = True
  | (yHead game) < -250 || (yHead game) > 250 = True
  | isSelfColliding (snakeLength game) (loc game) (prevLoc game) 0 = True
  | otherwise = False


isSelfColliding :: Float -> Location -> [Location] -> Float -> Bool
isSelfColliding _ _ [] _ = False
isSelfColliding len c1 (c2:t) acc
  | acc < len =  c1 == c2 || (isSelfColliding len c1 t (acc+1))
  | otherwise = False



main :: IO ()
main = play window background 60 initialState render handleKeys tick
