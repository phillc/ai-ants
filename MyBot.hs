module Main where

import Data.List
import Data.Maybe (mapMaybe)
import Control.Monad.State
import System.IO
import Data.Time.Clock

import Ants

data Memoizer = Memoizer {}
data Turn = Turn { memo :: Memoizer, gameState :: GameState }

newTurn :: GameState -> Turn
newTurn gs = Turn { memo = Memoizer
                  , gameState = gs
                  }

type T = State Turn

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

-- | Generates orders for an Ant in all directions
generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

{- |
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}
doEverything :: GameParams -> T [Order]
doEverything gp = do
  turn <- get
  -- generate orders for all ants belonging to me
  let gs = gameState turn
      generatedOrders = map generateOrders $ myAnts $ ants gs
  -- for each ant take the first "passable" order, if one exists
      orders = mapMaybe (tryOrder (world gs)) generatedOrders
  -- this shows how to check the remaining time
  --elapsedTime <- timeRemaining gs
  --hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders

doTurn :: GameParams -> GameState -> [Order]
doTurn gp gs = evalState (doEverything gp) (newTurn gs)

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
