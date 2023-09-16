{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.

import Data.Bifunctor (bimap, first, second)
import Data.Foldable (Foldable (..))
import Data.Maybe (isJust)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import qualified Data.Set as Set (difference, fromList, member, null, toList)
import RenderState (BoardInfo (..), CellType (..), DeltaBoard, Point)
import qualified RenderState as Board
import System.Random (Random (randomR), RandomGen (split), StdGen, uniformR)

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

type Unit = (Int, Int)

movementToUnit :: Movement -> Unit
movementToUnit North = (-1, 0)
movementToUnit South = (1, 0)
movementToUnit East = (0, 1)
movementToUnit West = (0, -1)

class Opposable a where
    opposite :: a -> a

instance Opposable Movement where
    opposite North = South
    opposite South = North
    opposite East = West
    opposite West = East

instance Opposable Unit where
    opposite (x, y) = (-x, -y)

{- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
  In first sight we'd define the snake as a sequence, but If you think carefully, an empty
  sequence can't represent a valid Snake, therefore we must use a non empty one.
  You should investigate about Seq type in haskell and we it is a good option for our porpouse.
-}
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

{- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and
  a random seed to calculate the next random apple.
-}
data GameState = GameState
    { snakeSeq :: SnakeSeq
    , applePosition :: Point
    , movement :: Movement
    , randomGen :: StdGen
    }
    deriving (Show, Eq)

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement = opposite

-- >>> opositeMovement North == South
-- >>> opositeMovement South == North
-- >>> opositeMovement East == West
-- >>> opositeMovement West == East
-- True
-- True
-- True
-- True

{- | Purely creates a random point within the board limits
  You should take a look to System.Random documentation.
  Also, in the import list you have all relevant functions.
-}
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint boardInfo = randomR ((1, 1), (boardInfo.height, boardInfo.width))

{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}

-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq -> Bool
inSnake p snake = p `elem` snake.snakeBody || p == snake.snakeHead

{-
This is a test for inSnake. It should return
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq
-- True
-- True
-- False

{- | Calculates de new head of the snake. Considering it is moving in the current direction
  Take into acount the edges of the board
-}
nextHead :: BoardInfo -> GameState -> Point
nextHead boardInfo gameState = (r, c)
  where
    (r', c') =
        let (dr, dc) = movementToUnit gameState.movement
         in bimap (+ dr) (+ dc) gameState.snakeSeq.snakeHead
    r
        | r' < 1 = height boardInfo
        | r' > height boardInfo = 1
        | otherwise = r'
    c
        | c' < 1 = width boardInfo
        | c' > width boardInfo = 1
        | otherwise = c'

{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)
-- True
-- True
-- True

-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, StdGen)
-- NOTE: Exercise prompt seems to nudge use toward using makeRandomPoint & inSnake, but I don't know how reasonable
--       that is since it seems inefficient to keep generating random points until we find one that works.
newApple boardInfo (GameState snake oldApplePosition _ gen)
    -- HACK: Haven't looked into the condition where there are no more valid points left
    | Set.null validPoints = error "No more points available"
    | otherwise =
        let (i, gen') = randomR (0, length validPoints - 1) gen
         in (Set.toList validPoints !! i, gen')
  where
    invalidPoints = Set.fromList $ oldApplePosition : snake.snakeHead : toList snake.snakeBody
    allPoints = Set.fromList [(r, c) | r <- [1 .. boardInfo.height], c <- [1 .. boardInfo.width]]
    validPoints = Set.difference allPoints invalidPoints

{- We can't test this function because it depends on makeRandomPoint -}

{- | Moves the snake based on the current direction. It sends the adequate RenderMessage
Notice that a delta board must include all modified cells in the movement.
For example, if we move between this two steps
       - - - -          - - - -
       - 0 $ -    =>    - - 0 $
       - - - -    =>    - - - -
       - - - X          - - - X
We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]

Another example, if we move between this two steps
       - - - -          - - - -
       - - - -    =>    - X - -
       - - - -    =>    - - - -
       - 0 $ X          - 0 0 $
We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-}
move :: BoardInfo -> GameState -> (Board.RenderMessage, GameState)
move boardInfo gameState@(GameState snake oldApplePosition oldMovement _)
    | inSnake newHead snake = (Board.GameOver, gameState)
    | otherwise = (Board.RenderBoard deltaBoard, GameState newSnake newApplePosition oldMovement gen')
  where
    newHead = nextHead boardInfo gameState
    (newApplePosition, gen') = newApple boardInfo gameState
    newSnake = SnakeSeq newHead $ snake.snakeHead <| if newHead /= oldApplePosition then S.deleteAt (length snake.snakeBody - 1) snake.snakeBody else snake.snakeBody
    deltaBoard = [(newHead, SnakeHead), (snake.snakeHead, Snake)] ++ if newHead /= oldApplePosition then [(snake.snakeBody `S.index` (length snake.snakeBody - 1), Board.Empty)] else [(newApplePosition, Apple)]

{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
-- RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
-- RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,2),Apple)]
-- RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]
