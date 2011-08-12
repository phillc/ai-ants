module Main where

import Control.Monad.State
import Data.List
import Data.Maybe (fromJust, isNothing, isJust, fromMaybe)
import Data.Ord (comparing)
import System.IO
import System.Random (StdGen, newStdGen, randoms)

import Ants

import Debug.Trace

type GS = State GameState

simulateOrder :: Order -> Point
simulateOrder order = move (direction order) (point $ ant order)

instance Show GameState where
  show (GameState _ a f o ) = show (a, f, o)
  --show (GameState w a f o) = show ((renderWorld w), a, f, o)

unsort :: StdGen -> [x] -> [x]
unsort g es = map snd . sortBy (comparing fst) $ zip rs es
  where rs = randoms g :: [Integer]

unoccupied :: GameState -> Point -> Bool
unoccupied gs p = not $ any (== p) (map point $ myAnts gs)

approachable :: GameState -> Order -> Bool
approachable gs order = (passable (world gs) order) && (unoccupied gs (simulateOrder order))

occupiable :: GameState -> Point -> Bool
occupiable gs p =
  let w = world gs
  in tile (w %! p) /= Water && unoccupied gs p

distance' :: GameParams -> World -> Point -> Point -> Int
distance' gp w p1 p2 = case shortestPath gp w p1 p2 of
                         Nothing -> 100
                         Just path -> length path

moveAnt :: Ant -> Direction -> GS [Order]
moveAnt ant direction = do
  state <- get
  let myAnts' = myAnts state
      otherAnts = delete ant myAnts'
  put state{myAnts = (ImmobileAnt (move direction (point ant)) (owner ant)) : otherAnts}
  return [Order ant direction]

moveOrImmobilizeAnt :: Ant -> Direction -> GS [Order]
moveOrImmobilizeAnt ant direction = do
  unoccupied' <- gets $ flip unoccupied (move direction (point ant))
  orders <- if unoccupied' then moveAnt ant direction else immobilizeAnt ant
  return orders

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
      radius = 100
      paths = foldl (\ps p -> case fst p of
                                Nothing -> ps
                                Just ss -> (ss, snd p):ps)
                    []
                    $ concatMap (\f -> [(shortestPath gp w (point ant) f, (ant, f)) | ant <- antsAroundPoint radius f (myMobileAnts state)]) fs
      shortestPaths = sortBy (comparing (length . fst)) paths
  orders <- sendClosestAnt' shortestPaths
  return orders
  where
    sendClosestAnt' :: [([Point], (Ant, Food))] -> GS [Order]
    sendClosestAnt' paths
      | null paths = do
          return []
      | otherwise = do
          state <- get
          w <- gets world
          let shortestDistance = head paths
              path = fst shortestDistance
              moveAntTo = head path
              moveDirection = directionOf w ant moveAntTo
              ant = fst $ snd shortestDistance
              f = snd $ snd shortestDistance
              remainingFood = delete f fs
          orders <- moveOrImmobilizeAnt ant moveDirection
          remainingOrders <- (sendClosestAnt gp remainingFood)
          return $ orders ++ remainingOrders

attackFood :: GameParams -> GS [Order]
attackFood gp = do
  state <- get
  sendClosestAnt gp (food state)

antsAroundPoint :: Int -> Point -> [Ant] -> [Ant]
antsAroundPoint radius location allAnts =
  let pointsAroundAnt = pointsAroundPoint radius location
  in filter (\a -> point a `elem` pointsAroundAnt) allAnts

pointsAroundPoint :: Int -> Point -> [Point]
pointsAroundPoint radius location = map (sumPoint location) $ getPointCircle radius

spreadOut :: GameParams -> StdGen -> GS [Order]
spreadOut gp gen = do
  ants' <- gets myMobileAnts
  orders <- liftM concat $ mapM moveAway ants'
  return orders
  where
    radius = 100
    moveAway :: Ant -> GS [Order]
    moveAway movingAnt = do
      state <- get
      allAnts <- gets myAnts
      let location = point movingAnt
          antsAroundAnt = antsAroundPoint radius location allAnts
          -- If this took into consideration # of land/water tiles, number of unknown tiles, perhaps we can spread out even more optimally
          counts = [ (length $ filter (\a -> row (point a) < row location) antsAroundAnt, South)
                   , (length $ filter (\a -> row (point a) > row location) antsAroundAnt, North)
                   , (length $ filter (\a -> col (point a) > col location) antsAroundAnt, West)
                   , (length $ filter (\a -> col (point a) < col location) antsAroundAnt, East)
                   ]
          counts' = unsort gen counts
          decision = find (\cs -> occupiable state (move (snd cs) location)) (reverse $ sortBy (comparing fst) counts')
      if isJust decision then moveOrImmobilizeAnt movingAnt (snd (fromJust decision)) else immobilizeAnt movingAnt

avoidEnemies :: GameParams -> StdGen -> GS [Order]
avoidEnemies gp gen = do
  ants' <- gets myMobileAnts
  orders <- liftM concat $ mapM moveAway ants'
  return orders
  where
    radius = 17
    moveAway :: Ant -> GS [Order]
    moveAway movingAnt = do
      state <- get
      allAnts <- gets enemyAnts
      let location = point movingAnt
          antsAroundAnt = antsAroundPoint radius location allAnts
          -- If this took into consideration # of land/water tiles, number of unknown tiles, perhaps we can spread out even more optimally
          counts = [ (length $ filter (\a -> row (point a) < row location) antsAroundAnt, South)
                   , (length $ filter (\a -> row (point a) > row location) antsAroundAnt, North)
                   , (length $ filter (\a -> col (point a) > col location) antsAroundAnt, West)
                   , (length $ filter (\a -> col (point a) < col location) antsAroundAnt, East)
                   ]
          counts' = unsort gen $ filter ((>0) . fst) counts
          decision = find (\cs -> occupiable state (move (snd cs) location)) (reverse $ sortBy (comparing fst) counts')
      if isJust decision then moveOrImmobilizeAnt movingAnt (snd (fromJust decision)) else return []

doEverything :: GameParams -> StdGen -> GS [Order]
doEverything gp gen = do
  --orders1 <- attack
  -- orders1 <- handle 2v1
  orders1 <- avoidEnemies gp gen
  -- group ants
  orders2 <- attackFood gp --minimax food
  orders3 <- spreadOut gp gen

  return $ orders1 ++ orders2 ++ orders3

doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  gen <- newStdGen

  return $ evalState (doEverything gp gen) gs

-- | This runs the game
main :: IO ()
main = game doTurn

