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

instance Show Turn where
  show (Turn _ a f o) = show (a, f, o)
  --show (Turn w a f o) = show ((renderWorld w), a, f, o)

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

createFuture :: Turn -> [Order] -> Turn
createFuture turn os =
  let newAnts = applyOrders (world turn) (ants turn) os
      os' = orders turn
  in Turn { world = world turn
          , orders = os ++ os'
          , ants = newAnts
          , food = food turn
          }

moveable :: Ant -> Bool
moveable (MobileAnt _ _) = True
moveable _ = False

unoccupied :: Turn -> Point -> Bool
unoccupied turn p = not $ any (== p) (map point $ myAnts $ ants turn)

approachable :: Turn -> Order -> Bool
approachable turn order = (passable (world turn) order) && (unoccupied turn (simulateOrder order))

circularStrategy :: [Direction] -> Turn -> Turn
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

clockwiseStrategy :: Turn -> Turn
clockwiseStrategy = circularStrategy [North, East, South, West]

clockwiseStrategy' :: Turn -> Turn
clockwiseStrategy' = circularStrategy [South, East, North, West]

counterClockwiseStrategy :: Turn -> Turn
counterClockwiseStrategy = circularStrategy [West, South, East, North]

counterClockwiseStrategy' :: Turn -> Turn
counterClockwiseStrategy' = circularStrategy [East, South, West, North]

surroundingPoints :: World -> Point -> [Point]
surroundingPoints w p = filter (\p' -> tile (w %! p') /= Water) [w %!% move d p | d <- [North, West, East, South]]

distance' :: GameParams -> World -> Point -> Point -> Int
distance' gp w p1 p2 = case shortestPath gp w p1 p2 of
--distance' gp w p1 p2 = case (trace ("the shortestPath was" ++ show foo) foo) of
                         Nothing -> 100
                         Just path -> length path
   where foo = shortestPath gp w p1 p2

shortestPath :: GameParams -> World -> Point -> Point -> Maybe [Point]
--shortestPath gp w p1 p2 = trace ("shortestPath " ++ show p1 ++ " " ++ show p2)  $ aStar surroundingPoints' distanceOfNeighbor heuristic isGoal startingPoint
shortestPath gp w p1 p2 = aStar surroundingPoints' distanceOfNeighbor heuristic isGoal startingPoint
  --where surroundingPoints' pp = trace ("surrounding points of " ++ show pp ++ " is " ++ ( show $ S.fromList $ surroundingPoints w pp )) (S.fromList $ surroundingPoints w pp)
  where surroundingPoints' = S.fromList . surroundingPoints w
        distanceOfNeighbor _ _ = 1
        heuristic = distance gp p2
        -- heuristic otherPoint= trace ("checking heurstic distance between " ++ show p2 ++ " and " ++ show otherPoint) $ distance gp p2 otherPoint
        isGoal p' = p' == p2
        startingPoint = p1

evaluate :: GameParams -> Turn -> Int
evaluate gp turn =
  let numAnts = length $ ants turn
  --let numAnts = trace "running evaluation" $ length $ ants turn
      distances = [distance' gp (world turn) food (point ant) | food <- (food turn), ant <- (myAnts $ ants turn)]
      shortestDistance = if null distances then
                           0
                         else
                           head $ sort distances
      sumDistances = foldr (+) 0 distances
  in numAnts - sumDistances - (shortestDistance * 50)

doTurn :: GameParams -> UTCTime -> Turn -> IO [Order]
doTurn gp startTime gs = do
  let futures = map (\s -> s gs) [counterClockwiseStrategy, clockwiseStrategy, clockwiseStrategy', counterClockwiseStrategy']
      evaluations = sortBy (comparing (((-1) *) . fst)) [(evaluate gp f, f) | f <- futures]
      orders' = orders (snd (head evaluations))

  -- this shows how to check the remaining time
  hPutStrLn stderr $ show orders'
  elapsedTime <- timeRemaining startTime
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders' --orders'

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
