module Main where

import Data.List
import Data.Maybe (mapMaybe)
import System.IO

import Ants

validOrders :: World -> [Order] -> [Order]
validOrders w = filter (passable w)

generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

-- tmpDist :: (Order, Food) -> Int
-- tmpDist order food = move (direction order) (point $ ant order)

distanceToFood :: GameParams -> (Order, Food) -> (Order, Food) -> Ordering
distanceToFood gp (order1, food1) (order2, food2) =
  let location1 = move (direction order1) (point $ ant order1)
      location2 = move (direction order2) (point $ ant order2)
      distance1 = distance gp location1 food1
      distance2 = distance gp location2 food2
  in distance1 `compare` distance2

firstPerAnt :: [(Order, Food)] -> [Order]
firstPerAnt [] = []
firstPerAnt (combo:combos) = [fst combo]

combos :: World -> [Ant] -> [Food] -> [(Order, Food)]
combos _ [] _ = []
combos world (ant:ants) foods = 
  let orders = validOrders world $ generateOrders ant
  in (combos' orders foods) ++ (combos world ants foods)

combos' :: [Order] -> [Food] -> [(Order, Food)]
combos' _ [] = []
combos' [] _ = []
combos' (order:orders) foods = (map (\food -> (order, food)) foods) ++ (combos' orders foods)

doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  let possibleOrders = combos (world gs) (myAnts (ants gs)) (food gs)
      sortedOrders = sortBy (distanceToFood gp) possibleOrders
      orders = firstPerAnt sortedOrders
      
  hPutStrLn stderr $ show possibleOrders
  hPutStrLn stderr $ show sortedOrders
  hPutStrLn stderr $ show $ head sortedOrders
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
