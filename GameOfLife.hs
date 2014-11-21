module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Bitfield
import Control.Applicative ((<$>))
import Data.List
import System.Environment
import qualified World

data Board = Board {
    speed :: Int
  , windowSize :: (Int, Int)
  , gridSize :: (Int, Int)
  , liveOn :: [Int]
  , giveBirth :: [Int]
  , seed :: Integer
} deriving (Eq, Show, Read)

main = getArgs >>= \args -> case args of
    [file] -> read <$> readFile file >>= gameOfLife
    _ -> gameOfLife (Board 10 (800,600) (80,60) [2,3] [3] 0)

gameOfLife :: Board -> IO ()
gameOfLife board@(Board speed windowSize gridSize liveOn giveBirth seed) = playIO
    (InWindow "Game of Life" windowSize (100, 100))
    white
    speed
    (World.new gridSize seed)
    (return . renderWorld)
    handleEvent
    (\delta -> return . nextWorld delta)
  where

    (gridWidth, gridHeight) = gridSize
    (width, height) = windowSize

    handleEvent event world = case event of

        EventKey (MouseButton _) Up _ (x, y) ->
            return $ world { World.cells = toggle (World.cells world) gridPos }
                where gridPos = ( round ((x - cellOffsetX) / cellWidth)
                                , round ((y - cellOffsetY) / cellHeight) )

        EventKey (SpecialKey KeySpace) Up _ _ ->
            return $ world { World.playing = (not (World.playing world)) }

        EventKey (SpecialKey KeyTab) Up _ _ -> do
            let board' = board { seed = bitfieldToInteger (World.cells world) }
            writeFile "world.txt" (show board')
            return world

        _ -> return world

    nextWorld delta world = case World.playing world of
        True -> world { World.cells = cells' }
          where
            cells  = World.cells world
            cells' = foldl' updateCell cells allCells
            updateCell cs c@(cx, cy) = case cells `isSet` c of
                True  | not stayAlive -> cs `clear` c
                False | beBorn        -> cs `set` c
                _ -> cs
              where
                neighbours = delete (cx, cy) [ (x `mod` gridWidth, y `mod` gridHeight)
                                             | x <- [pred cx..succ cx]
                                             , y <- [pred cy..succ cy] ]
                numAlives = length $ filter id $ map (isSet cells) neighbours
                stayAlive = numAlives `elem` liveOn
                beBorn    = numAlives `elem` giveBirth

        _ -> world -- do not advance if not playing

    renderWorld world =
        Pictures $ foldl' paintCell [] allCells
      where
        paintCell pics p@(x, y) = case cells `isSet` p of
            True -> renderCell x y : pics
            _    -> pics
        cells = World.cells world

    allCells = [ (x, y) | x <- [0..pred gridWidth], y <- [0..pred gridHeight] ]

    renderCell x y = Translate (xF * cellWidth  + cellOffsetX)
                               (yF * cellHeight + cellOffsetY)
                   $ rectangleSolid cellWidth cellHeight
      where
        (xF, yF) = (fromIntegral x, fromIntegral y)

    [gridWidthF, gridHeightF, widthF, heightF] = map fromIntegral [gridWidth, gridHeight, width, height]

    cellOffsetX = negate (widthF  / 2) + cellWidth  / 2
    cellOffsetY = negate (heightF / 2) + cellHeight / 2

    cellWidth = widthF / gridWidthF
    cellHeight = heightF / gridHeightF

