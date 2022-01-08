module Input where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import PongGame

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'r') _ _ _) game =
  game { ballLoc = (0, 0) }

handleKeys (EventKey (Char 'p') keyState _ _) game =
  handlePause keyState game
  
handleKeys (EventKey (SpecialKey KeyUp) keyState _ _) game =
  handlePaddle1Up keyState game
handleKeys (EventKey (SpecialKey KeyDown) keyState _ _) game =
  handlePaddle1Down keyState game

handleKeys (EventKey (Char 'w') keyState _ _) game =
  handlePaddle2Up keyState game
handleKeys (EventKey (Char 's') keyState _ _) game =
  handlePaddle2Down keyState game 

handleKeys _ game = game

handlePause :: KeyState -> PongGame -> PongGame
handlePause Up   game = game { paused = not $ paused game } 
handlePause Down game = game

handlePaddle1Up :: KeyState -> PongGame -> PongGame
handlePaddle1Up Down game = game { player1UpPressed = True}  
handlePaddle1Up Up   game = game { player1UpPressed = False}  

handlePaddle1Down :: KeyState -> PongGame -> PongGame
handlePaddle1Down Down game = game { player1DownPressed = True}  
handlePaddle1Down Up   game = game { player1DownPressed = False}  

handlePaddle2Up :: KeyState -> PongGame -> PongGame
handlePaddle2Up Down game = game { player2UpPressed = True}  
handlePaddle2Up Up   game = game { player2UpPressed = False}  

handlePaddle2Down :: KeyState -> PongGame -> PongGame
handlePaddle2Down Down game = game { player2DownPressed = True}  
handlePaddle2Down Up   game = game { player2DownPressed = False} 
