module Main where

import Data.List
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.IO

import Ants




import Data.Array

simulateOrder :: Order -> Point
simulateOrder order = move (direction order) (point $ ant order)

instance Show Turn where
  show (GameState w a f _) = show (a, f)
  --show (GameState w a f _) = show ((renderWorld w), a, f)
  show (Future w a f o) = show (a, f, o)
  --show (Future w a f o) = show ((renderWorld w), a, f, o)

applyOrders :: [Ant] -> [Order] -> [Ant]
applyOrders ants [] = ants
applyOrders ants (order:os) =
  let newAnt = ImmobileAnt (simulateOrder order) (owner $ ant order)
      otherAnts = filter (/= (ant order)) ants
  in [newAnt] ++ (applyOrders otherAnts os)

createFuture :: Turn -> [Order] -> Turn
createFuture turn os =
  let newAnts = applyOrders (ants turn) os
      os' = case turn of
                (GameState _ _ _ _) -> []
                (Future _ _ _ os) -> os
  in Future { world = world turn
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
approachable turn order = (passable (world turn) order) -- && (unoccupied turn (simulateOrder order))


circularStrategy :: [Direction] -> Turn -> Turn
circularStrategy directions turn
  | null ants' = createFuture turn []
  | null moveableAnts = createFuture turn []
  | otherwise =
      let theAnt = head $ moveableAnts
          generatedOrders = map (Order theAnt) directions
          orders = head $ filter (approachable turn) generatedOrders
      in circularStrategy directions (createFuture turn [orders])
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
      sumDistances = foldr (+) 0 [distance gp food (point ant) | food <- (food turn), ant <- (ants turn)]
  in numAnts - sumDistances

doTurn :: GameParams -> Turn -> IO [Order]
doTurn gp gs = do
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
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return orders' --orders'

-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
