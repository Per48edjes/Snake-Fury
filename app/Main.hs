
module Main where

import qualified Board
import qualified Snake
import Board (BoardInfo(BoardInfo), updateMessages)
import System.Random ( getStdGen, randomRIO )
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Control.Concurrent
    ( forkIO, newEmptyMVar, threadDelay, newMVar )
import System.IO (stdin, hSetBuffering, BufferMode (NoBuffering), hSetEcho, stdout)
import Control.Concurrent.BoundedChan
    ( newBoundedChan )
import qualified Data.ByteString.Builder as B
import EventQueue
    ( readEvent,
      writeClock,
      writeSpeed,
      writeUserInput,
      Clock(Tick),
      Event(UserEvent, ClockEvent),
      EventQueue(EventQueue) )

getRandomPoint :: Int -> Int -> IO Board.Point
getRandomPoint h w = (,) <$> randomRIO (1, h) <*> randomRIO (1, w)

inititalizePoints :: Int -> Int -> IO (Board.Point, Board.Point)
inititalizePoints h w = do
  (snakeInit, appleInit) <- (,) <$> getRandomPoint h w <*> getRandomPoint h w
  if snakeInit == appleInit
    then inititalizePoints h w
    else return (snakeInit, appleInit)

gameInitialization :: Int -> Int -> Int -> IO (Snake.AppState, Board.RenderState , EventQueue)
gameInitialization hight width initialspeed = do
  (snakeInit, appleInit) <- inititalizePoints hight width
  sg <- getStdGen
  newUserEventQueue <- newBoundedChan 3
  newClock <- newEmptyMVar
  newSpeed <- newMVar (initialspeed, initialspeed)
  let binf = BoardInfo hight width
      gameState = Snake.AppState (Snake.SnakeSeq snakeInit S.Empty) appleInit Snake.North binf sg
      renderState = Board.buildInitialBoard binf snakeInit appleInit
      eventQueue = EventQueue newClock newUserEventQueue newSpeed
  return (gameState, renderState, eventQueue)

gameloop :: Snake.AppState -> Board.RenderState -> EventQueue -> IO ()
gameloop app b queue =  do
    currentSpeed <- writeSpeed (Board.score b) queue
    threadDelay currentSpeed
    event <- readEvent queue
    let (app',deltas) =
          case event of
                ClockEvent Tick -> Snake.move app
                UserEvent move ->
                  if Snake.movement app == Snake.opositeMovement move
                    then Snake.move app
                    else Snake.move $ app {Snake.movement = move}
        board' = b `Board.updateMessages` deltas
    putStr "\ESC[2J"
    B.hPutBuilder stdout $ Board.render board'
    gameloop app' board' queue

main :: IO ()
main = do
    -- enable reading key strokes
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    
    -- Game Initializacion
    [h, w, timeSpeed] <- fmap read <$> getArgs
    (gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

    -- Game Loop. We run three different threads, one for the clock, one for the gameloop and one for user inputs.
    _ <- forkIO $ writeClock eventQueue
    _ <- forkIO $ gameloop gameState renderState eventQueue
    writeUserInput eventQueue
