{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent (
    forkIO,
    threadDelay,
 )
import Control.Monad (unless)
import Data.ByteString.Builder (hPutBuilder)
import EventQueue (Event (Tick, UserEvent), EventQueue, readEvent, setSpeed, writeUserInput)
import GameState (GameState (movement), move, opositeMovement)
import Initialization (gameInitialization)
import RenderState (BoardInfo, RenderState (gameOver, score), render, updateRenderState)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, hSetEcho, stdin, stdout)

-- The game loop is easy:
--   - wait some time
--   - read an Event from the queue
--   - Update the GameState
--   - Update the RenderState based on message delivered by GameState update
--   - Render into the console
gameloop :: BoardInfo -> GameState -> RenderState -> EventQueue -> IO ()
gameloop binf gstate rstate queue = do
    newSpeed <- setSpeed rstate.score queue
    threadDelay newSpeed
    event <- readEvent queue
    let (delta, gstate') =
            case event of
                Tick -> move binf gstate
                UserEvent m ->
                    if movement gstate == opositeMovement m
                        then move binf gstate
                        else move binf $ gstate{movement = m}
    let rstate' = updateRenderState rstate delta
        isGameOver = gameOver rstate'
    putStr "\ESC[2J" -- This cleans the console screen
    hPutBuilder stdout $ render binf rstate'
    unless isGameOver $ gameloop binf gstate' rstate' queue

-- | main.
main :: IO ()
main = do
    -- enable reading key strokes
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True

    -- Game Initializacion
    [h, w, fps] <- fmap read <$> getArgs
    let timeSpeed = 1_000_000 `div` fps -- One second is 1_000_000 microseconds, which is the unit used by GHC internally.
    (binf, gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

    -- Game Loop. We run two different threads, one for the gameloop (main) and one for user inputs.
    _ <- forkIO $ writeUserInput eventQueue
    let initialState = gameState
    gameloop binf initialState renderState eventQueue
