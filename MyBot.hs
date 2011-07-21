module Main where

--import Data.Array
import Data.List
import Data.Maybe (fromJust, isNothing)
import Data.Ord (comparing)
import Data.Time.Clock
import qualified Data.Set as S
import System.IO
import Debug.Trace

import Ants
import AStar

simulateOrder :: Order -> Point
simulateOrder order = move (direction order) (point $ ant order)

instance Show GameState where
  show (GameState _ a f o _) = show (a, f, o)
  --show (GameState w a f o) = show ((renderWorld w), a, f, o)

applyOrders :: World -> [Ant] -> [Order] -> [Ant]
applyOrders w ants [] = ants
applyOrders w ants (order:os) =
  let oldAnt = ant order
      newAnt = ImmobileAnt (simulateOrder order) (owner oldAnt)
      createdAnts = if tile (w %! (point newAnt)) == FoodTile then
                      [ImmobileAnt (point oldAnt) (owner oldAnt)]
                    else
                      []
      otherAnts = filter (/= oldAnt) ants
  in createdAnts ++ [newAnt] ++ (applyOrders w otherAnts os)

createFuture :: GameState -> [Order] -> GameState
createFuture turn os =
  let newAnts = applyOrders (world turn) (ants turn) os
      os' = orders turn
  in turn { orders = os ++ os'
          , ants = newAnts
          }

moveable :: Ant -> Bool
moveable (MobileAnt _ _) = True
moveable _ = False

unoccupied :: GameState -> Point -> Bool
unoccupied turn p = not $ any (== p) (map point $ myAnts $ ants turn)

approachable :: GameState -> Order -> Bool
approachable turn order = (passable (world turn) order) && (unoccupied turn (simulateOrder order))

circularStrategy :: [Direction] -> GameState -> GameState
circularStrategy directions turn
  | null ants' = createFuture turn []
  | null moveableAnts = createFuture turn []
  | otherwise =
      let theAnt = head $ moveableAnts
          generatedOrders = map (Order theAnt) directions
          order = find (approachable turn) generatedOrders
      in if isNothing order then
           circularStrategy directions (turn{ants = (ImmobileAnt (point theAnt) (owner theAnt) : (tail moveableAnts))})
         else
           circularStrategy directions (createFuture turn [fromJust order])
  where ants' = myAnts $ ants turn
        moveableAnts = filter moveable ants'

clockwiseStrategy = circularStrategy [North, East, South, West]
clockwiseStrategy' = circularStrategy [South, East, North, West]
counterClockwiseStrategy = circularStrategy [West, South, East, North]
counterClockwiseStrategy' = circularStrategy [East, South, West, North]


distance' :: GameParams -> GameState -> Point -> Point -> Int
distance' gp gs p1 p2 = case shortestPath gp gs p1 p2 of
                         Nothing -> 100
                         Just path -> length path

shortestPath :: GameParams -> GameState -> Point -> Point -> Maybe [Point]
shortestPath gp gs p1 p2 = aStar surroundingPoints' distanceOfNeighbor heuristic isGoal startingPoint
  where surroundingPoints' = S.fromList . surroundingPoints gs
        distanceOfNeighbor _ _ = 1
        heuristic = distance gp p2
        isGoal p' = p' == p2
        startingPoint = p1

evaluate :: GameParams -> GameState -> Int
evaluate gp gs =
  let numAnts = length $ ants gs
      distances = [distance' gp gs food (point ant) | food <- (food gs), ant <- (myAnts $ ants gs)]
      shortestDistance = if null distances then
                           0
                         else
                           head $ sort distances
      sumDistances = foldr (+) 0 distances
  in numAnts - sumDistances - (shortestDistance * 50)

doTurn :: GameParams -> UTCTime -> GameState -> IO [Order]
doTurn gp startTime gs = do
  let futures = map (\s -> s gs) [counterClockwiseStrategy, clockwiseStrategy, clockwiseStrategy', counterClockwiseStrategy']
      evaluations = sortBy (comparing (((-1) *) . fst)) [(evaluate gp f, f) | f <- futures]
      orders' = orders (snd (head evaluations))

  -- this shows how to check the remaining time
  hPutStrLn stderr $ show orders'
  elapsedTime <- timeRemaining startTime
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders'

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
