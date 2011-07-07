module Main where

import Data.List
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.IO

import qualified Ants
import Ants hiding (world, ants, food)

class Turn a where
  world :: a -> World
  ants :: a -> [Ant]
  food :: a -> [Food]

data Future = Future
  { fworld :: World
  , fants :: [Ant]
  , ffood :: [Food]
  , forders :: [Order]
  } deriving (Show)

instance Turn Future where
  world = fworld
  ants = fants
  food = ffood

instance Turn GameState where
  world = Ants.world
  ants = Ants.ants
  food = Ants.food

simulateOrder :: Order -> Point
simulateOrder order = move (direction order) (point $ ant order)

applyOrders :: [Ant] -> [Order] -> [Ant]
applyOrders ants [] = ants
applyOrders ants (order:orders) = 
  let newAnt = Ant (simulateOrder order) (owner $ ant order)
      otherAnts = filter (/= (ant order)) ants
  in [newAnt] ++ (applyOrders otherAnts orders)

createFuture :: (Turn a) => a -> [Order] -> Future
createFuture turn orders =
  let newAnts = applyOrders (ants turn) orders
  in Future { fworld = world turn
            , forders = orders
            , fants = newAnts
            , ffood = food turn
            }

circularStrategy :: (Turn a) => [Direction] -> a -> Future
circularStrategy directions turn =
  let myAnts' = myAnts $ ants turn
      generatedOrders = map (\ant -> map (Order ant) directions) myAnts'
      orders = head $ map (filter (passable $ world turn)) generatedOrders
  in createFuture turn orders

clockwiseStrategy :: (Turn a) => a -> Future
clockwiseStrategy = circularStrategy [North .. West]

counterClockwiseStrategy :: (Turn a) => a -> Future
counterClockwiseStrategy = circularStrategy [West .. North]

evaluate :: (Turn a) => GameParams -> a -> Int
evaluate gp turn =
  let ants' = ants turn
      foods = food turn
      numAnts = length ants'
      sumDistances = foldr (+) 0 [distance gp food (point ant) | food <- foods, ant <- ants']
  in numAnts - sumDistances

doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  let futures = map (\s -> s gs) [clockwiseStrategy, counterClockwiseStrategy]
      evaluations = [(evaluate gp f, f) | f <- futures]
      orders = forders (snd (head (sortBy (comparing fst) evaluations )))

  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
