data Tile = Floor | Chasm | Snake
  deriving Show

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)

type Point = (Integer, Integer)
type GameMap = Point -> Tile

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves gameMap steps point = fmap (evalPoint gameMap) (points gameMap steps [point] [])

-- Check other solutions
points :: GameMap -> Int -> [Point] -> [Point] -> [Point]
points gameMap 0 floors deaths = floors ++ deaths
points gameMap steps currentFloors currentDeaths = points gameMap (steps - 1) nextFloors nextDeaths
  where nextDeaths = (filter (\point -> (evalPoint gameMap point) /= (Right point)) currentFloors) ++ currentDeaths
        floors = filter (\point -> (evalPoint gameMap point) == (Right point)) currentFloors
        nextFloors = (concatMap getNextPoints floors)

getNextPoints :: Point -> [Point]
getNextPoints (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

evalPoint :: GameMap -> Point -> Either DeathReason Point
evalPoint gameMap point = case (gameMap point) of
  Floor -> Right point
  Snake -> Left Poisoned
  Chasm -> Left Fallen

moveToNextPoint :: GameMap -> Either DeathReason Point -> Point -> Either DeathReason Point
moveToNextPoint gameMap currentMove nextPoint = currentMove *> (evalPoint gameMap nextPoint)

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie reason gameMap steps point = length $ filter ( == (Left reason)) (moves gameMap steps point)

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm

--  | 0 1 2 3 4 5
-- --------------
-- 0| o o o o o o
-- 1| o       s o
-- 2| o   s     o
-- 3| o         o
-- 4| o         o
-- 5| o o o o o o

-- GHCi> waysToDie Poisoned map1 1 (4,2)
-- 1  -- можно пойти к змее наверх
-- GHCi> waysToDie Poisoned map1 2 (4,2)
-- 2  -- можно пойти к змее наверх или к змее влево
-- GHCi> waysToDie Poisoned map1 3 (4,2)
-- 5  -- за три шага к левой змее, по-прежнему можно дойти одним способом,
--    -- а к правой — уже четырьмя (вверх, влево-вверх-вправо,
--    --                            влево-вправо-вверх, вниз-вверх-вверх)
-- GHCi> waysToDie Poisoned map1 4 (4,2)
-- 13

-- moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
-- moves m n p = runExceptT $ foldr (>=>) pure (replicate n (ExceptT . move)) p
--   where
--     steps :: Point -> [Point]
--     steps (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

--     stepOn :: Point -> Either DeathReason Point
--     stepOn p = case m p of
--       Floor -> Right p
--       Chasm -> Left Fallen
--       Snake -> Left Poisoned

--     move :: Point -> [Either DeathReason Point]
--     move p = stepOn <$> steps p

-- waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
-- waysToDie r m n p = length $ filter (== Left r) $ moves m n p
