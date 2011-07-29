module Main where

import Data.List
import Data.Maybe (fromJust, isNothing)
import Data.Ord (comparing)
import Control.Monad.State
import System.IO
import Data.Time.Clock

import Ants

data Turn = Turn { }

newTurn :: Turn
newTurn = Turn { 
               }

type T = State Turn

simulateOrder :: Order -> Point
simulateOrder order = move (direction order) (point $ ant order)

instance Show GameState where
  show (GameState _ a f o ) = show (a, f, o)
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

mShortestPath :: GameParams -> World -> Point -> Point -> Maybe [Point]
mShortestPath = shortestPath

distance' :: GameParams -> World -> Point -> Point -> Int
distance' gp w p1 p2 = case mShortestPath gp w p1 p2 of
                          Nothing -> 100
                          Just path -> length path

evaluate :: GameParams -> GameState -> T Int
evaluate gp gs = do
  let numAnts = length $ ants gs
      w = world gs
      distances = [distance' gp w food (point ant) | food <- (food gs), ant <- (myAnts $ ants gs)]
      shortestDistance = if null distances then
                           0
                         else
                           head $ sort distances
      sumDistances = foldr (+) 0 distances
  return $ numAnts - sumDistances - (shortestDistance * 5)

evaluations :: GameParams -> [GameState] -> T [(Int, GameState)]
evaluations gp gss = do
  state <- get
  let evals = [(evalState (evaluate gp f) state, f) | f <- gss]
  --return (sortBy (comparing (((-1) *) . fst)) evals)
  return evals

doEverything :: GameParams -> GameState -> T [Order]
doEverything gp gs = do
  let futures = map (\s -> s gs) [counterClockwiseStrategy, clockwiseStrategy, clockwiseStrategy', counterClockwiseStrategy']
  evals <- evaluations gp futures
  let orders' = orders (snd (head evals))
  -- this shows how to check the remaining time
  --elapsedTime <- timeRemaining gs
  --hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders'

doTurn :: GameParams -> GameState -> [Order]
doTurn gp gs = evalState (doEverything gp gs) newTurn

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
