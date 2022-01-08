module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Draw
import Input
import PongGame

main :: IO ()
main = let fps = 60
           windowWidth'  = windowWidth env
           windowHeight' = windowHeight env
           windowOffset' = windowOffset env
           background' = background env 
           window = InWindow "Pong" (windowWidth', windowHeight') (windowOffset', windowOffset')
       in play window background' fps initialState (render env) handleKeys (update env)

env :: Env 
env = Env 
  { windowWidth  = 600
  , windowHeight = 600
  , windowOffset = 400
  , background = black 
  , ballRadius = 10
  , paddleWidth = 20
  , paddleHeight = 80
  , paddleBorder = 5
  , wallWidth = 570
  , wallHeight = 10
  , paddleStep = 5
  , correctionLength = 60  -- -30 .. 0 .. 30   
  }

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (350, -40)
  , player1 = 0
  , player2 = 0
  , paused = False
  , player1UpPressed = False
  , player1DownPressed = False
  , player2UpPressed = False
  , player2DownPressed = False
  }
