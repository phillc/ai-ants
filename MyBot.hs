module Main where

--import Data.Array
import Data.List
import Data.Maybe (fromJust, isNothing)
import Data.Ord (comparing)
import Data.Time.Clock
import System.IO
import Debug.Trace

import Ants

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
createFuture gs os =
  let newAnts = applyOrders (world gs) (ants gs) os
      os' = orders gs
  in gs { orders = os ++ os'
        , ants = newAnts
        }

moveable :: Ant -> Bool
moveable (MobileAnt _ _) = True
moveable _ = False

unoccupied :: GameState -> Point -> Bool
unoccupied gs p = not $ any (== p) (map point $ myAnts $ ants gs)

approachable :: GameState -> Order -> Bool
approachable gs order = (passable (world gs) order) && (unoccupied gs (simulateOrder order))

circularStrategy :: [Direction] -> GameState -> GameState
circularStrategy directions gs
  | null ants' = createFuture gs []
  | null moveableAnts = createFuture gs []
  | otherwise =
      let theAnt = head $ moveableAnts
          generatedOrders = map (Order theAnt) directions
          order = find (approachable gs) generatedOrders
      in if isNothing order then
           circularStrategy directions (gs{ants = (ImmobileAnt (point theAnt) (owner theAnt) : (tail moveableAnts))})
         else
           circularStrategy directions (createFuture gs [fromJust order])
  where ants' = myAnts $ ants gs
        moveableAnts = filter moveable ants'

clockwiseStrategy = circularStrategy [North, East, South, West]
clockwiseStrategy' = circularStrategy [South, East, North, West]
counterClockwiseStrategy = circularStrategy [West, South, East, North]
counterClockwiseStrategy' = circularStrategy [East, South, West, North]


distance' :: GameParams -> GameState -> Point -> Point -> Int
distance' gp gs p1 p2 = case shortestPath gp gs p1 p2 of
                         Nothing -> 100
                         Just path -> length path

evaluate :: GameParams -> GameState -> Int
evaluate gp gs =
  let numAnts = length $ ants gs
      distances = [distance' gp gs food (point ant) | food <- (food gs), ant <- (myAnts $ ants gs)]
      shortestDistance = if null distances then
                           0
                         else
                           head $ sort distances
      sumDistances = foldr (+) 0 distances
  in numAnts - sumDistances - (shortestDistance * 5)

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
