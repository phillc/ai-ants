module Main where

import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing, isJust, fromMaybe)
import Data.Ord (comparing)
import Control.Monad.State
import System.IO
import Data.Time.Clock
import Ants

type GS = State GameState

simulateOrder :: Order -> Point
simulateOrder order = move (direction order) (point $ ant order)

instance Show GameState where
  show (GameState _ a f o ) = show (a, f, o)
  --show (GameState w a f o) = show ((renderWorld w), a, f, o)

moveable :: Ant -> Bool
moveable (MobileAnt _ _) = True
moveable _ = False

unoccupied :: GameState -> Point -> Bool
unoccupied gs p = not $ any (== p) (map point $ myAnts gs)

approachable :: GameState -> Order -> Bool
approachable gs order = (passable (world gs) order) && (unoccupied gs (simulateOrder order))

distance' :: GameParams -> World -> Point -> Point -> Int
distance' gp w p1 p2 = case shortestPath gp w p1 p2 of
                         Nothing -> 100
                         Just path -> length path

horizon = 4

moveAnt :: Ant -> Direction -> GS [Order]
moveAnt ant direction = do
  state <- get
  let myAnts' = myAnts state
      otherAnts = delete ant myAnts'
  put state{myAnts = (ImmobileAnt (move direction (point ant)) (owner ant)) : otherAnts}
  return [Order ant direction]

immobilizeAnt :: Ant -> GS [Order]
immobilizeAnt a@(MobileAnt p o) = do
  state <- get
  let myAnts' = myAnts state
      otherAnts = delete a myAnts'
  put state{myAnts = ((ImmobileAnt p o) : otherAnts)}
  return []


sendClosestAnt :: GameParams -> [Food] -> GS [Order]
sendClosestAnt gp fs = do
  state <- get
  let w = world state
      paths = foldl (\ps p -> case fst p of
                                Nothing -> ps
                                Just ss -> (ss, snd p):ps)
                    []
                    [(shortestPath gp w (point ant) f, (ant, f)) | ant <- myMobileAnts state, f <- food state]
      shortestPaths = sortBy (comparing (((-1) *) . length . fst)) paths
  orders <- sendClosestAnt' shortestPaths
  return orders
  where
    sendClosestAnt' :: [([Point], (Ant, Food))] -> GS [Order]
    sendClosestAnt' paths
      | null paths = do
          return []
      | otherwise = do
          state <- get
          let shortestDistance = head paths
              path = fst shortestDistance
              moveAntTo = head path
              ant = fst $ snd shortestDistance
              f = snd $ snd shortestDistance
              remainingFood = delete f fs
          orders <- if unoccupied state moveAntTo then ((moveAnt ant (directionOf ant moveAntTo))) else immobilizeAnt ant
          remainingOrders <- (sendClosestAnt gp remainingFood)
          return $ orders ++ remainingOrders

attackFood :: GameParams -> GS [Order]
attackFood gp = do
  state <- get
  sendClosestAnt gp (food state)

doEverything :: GameParams -> GS [Order]
doEverything gp = do
  --orders1 <- attack
  orders2 <- attackFood gp --minimax food
  --spreadOut

  return orders2

doTurn :: GameParams -> GameState -> [Order]
doTurn gp gs = evalState (doEverything gp) gs

-- | This runs the game
main :: IO ()
main = game doTurn

