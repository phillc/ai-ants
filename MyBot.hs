module Main where

--import Data.Array
import Data.List
import Data.Maybe (fromJust, isNothing)
import Data.Ord (comparing)
import Data.Time.Clock
import System.IO

import Ants
import Data.Graph.AStar

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

evaluate :: GameParams -> Turn -> Int
evaluate gp turn =
  let numAnts = length $ ants turn
      distances = [distance gp food (point ant) | food <- (food turn), ant <- (ants turn)]
      shortestDistance = if null distances then
                           0
                         else
                           head $ sort distances
      sumDistances = foldr (+) 0 distances
  in numAnts - sumDistances - (shortestDistance * 50)

doTurn :: GameParams -> UTCTime -> Turn -> IO [Order]
doTurn gp startTime gs = do
  let futures = map (\s -> s gs) [clockwiseStrategy', clockwiseStrategy, counterClockwiseStrategy', counterClockwiseStrategy]
      evaluations = sortBy (comparing (((-1) *) . fst)) [(evaluate gp f, f) | f <- futures]
      orders' = orders (snd (head evaluations))
  --hPutStrLn stderr $ "hmmm:"
  --hPutStrLn stderr $ show $ renderWorld $ world gs
  --hPutStrLn stderr $ "hmmm2:"
  --hPutStrLn stderr $ show $ createFuture gs []
  --hPutStrLn stderr $ show $ orders $ createFuture gs [head $ filter (approachable gs) $ map (Order $ head $ filter moveable $ myAnts $ ants gs) [North]]
  --
  hPutStrLn stderr $ show $ evaluations

  --hPutStrLn stderr $ "future:"
  --hPutStrLn stderr $ show $ circularStrategy [North] gs

  --hPutStrLn stderr $ "evaluations:"
  --hPutStrLn stderr $ show evaluations
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining startTime
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders' --orders'

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
