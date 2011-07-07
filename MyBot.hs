module Main where

import Data.List
import Data.Maybe (mapMaybe)
import System.IO

import Ants

validOrders :: World -> [Order] -> [Order]
validOrders w = filter (passable w)

generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

distanceToFood :: (Order, Food) -> (Order, Food) -> Ordering
distanceToFood pair1 pair2 = LT

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
      sortedOrders = sortBy distanceToFood possibleOrders
      orders = firstPerAnt sortedOrders
      
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
