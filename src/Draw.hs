module Draw where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import PongGame

-- TODO use Reader Env

-- | Convert a game state into a picture.
render :: Env 
       -> PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render env game = pictures 
  [ ball
  , walls
  , mkPaddle env rose (wallWidth env / 2) $ player1 game
  , mkPaddle env orange (- (wallWidth env / 2)) $ player2 game
  ]
    where
  --  The pong ball.
  ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid (ballRadius env)
  ballColor = dark red

  --  The bottom and top walls.
  wall :: Env -> Float -> Picture
  wall env offset =
    translate 0 offset $
      color wallColor $
        rectangleSolid (wallWidth env) (wallHeight env) 

  wallColor = greyN 0.5
  walls = pictures 
    [ wall env $ fromIntegral $ windowHeight env `div` 2
    , wall env $ - (fromIntegral $ windowHeight env `div` 2)
    ]

  --  Make a paddle of a given border and vertical offset.
  mkPaddle :: Env -> Color -> Float -> Float -> Picture
  mkPaddle env col x y = pictures
    [ translate x y $ color col $ rectangleSolid (paddleWidth env) (paddleHeight env)
    , translate x y $ color paddleColor $ 
        rectangleSolid (paddleWidth env - paddleBorder env) (paddleHeight env - paddleBorder env)
    ]

  paddleColor = light (light blue)