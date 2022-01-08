module PongGame where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

type Radius = Float
type Position = (Float, Float)
type VelCorrection = Float

-- | Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc :: (Float, Float)     -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)     -- ^ Pong ball (x, y) velocity. 
  , player1 :: Float -- ^ Left player paddle height.
                                  -- Zero is the middle of the screen. 
  , player2 :: Float -- ^ Right player paddle height.
  , paused  :: Bool
  , player1UpPressed :: Bool
  , player1DownPressed :: Bool
  , player2UpPressed :: Bool
  , player2DownPressed :: Bool
  } deriving Show

data Env = Env 
  { windowWidth  :: Int
  , windowHeight :: Int
  , windowOffset :: Int
  , background   :: Color
  , ballRadius :: Float
  , paddleWidth :: Float
  , paddleHeight :: Float
  , paddleBorder :: Float
  , wallWidth :: Float 
  , wallHeight :: Float 
  , paddleStep :: Float
  , correctionLength :: Float
  }

-- TODO use Reader Env

update :: Env -> Float -> PongGame -> PongGame
update env seconds game = 
  if paused game
    then game
    else   checkGameEnded env
         . movePaddle1 env
         . movePaddle2 env
        --  . checkPlayer2Pos
         . paddleBounce env
         . wallBounce env
         . moveBall seconds $
         game   

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
  where 
    (x,  y)  = ballLoc game
    (vx, vy) = ballVel game

    x' = x + vx * seconds  
    y' = y + vy * seconds  

paddleBounce :: Env -> PongGame -> PongGame
paddleBounce env game = 
  case paddleCollision env (ballLoc game) (ballRadius env) (player1 game) (player2 game) of 
    Just velCorrection -> let (vx, vy) = ballVel game
                              vy'      = vy + velCorrection  
                          in game { ballVel = (-vx, vy') }
    Nothing           -> game
   

paddleCollision :: Env -> Position -> Radius -> Float -> Float -> Maybe VelCorrection
paddleCollision env (x, y) radius p1 p2 =
  let 
    player1CollisionInX = x + radius >=   fromIntegral (windowWidth env)/ 2 - paddleWidth env
    player2CollisionInX = x - radius <= - fromIntegral (windowWidth env) / 2 + paddleWidth env
 
    playerCollisionInY p =    y >= p - paddleHeight env / 2 
                           && y <= p + paddleHeight env / 2

    player1Collision = player1CollisionInX && playerCollisionInY p1                       
    player2Collision = player2CollisionInX && playerCollisionInY p2    

    correction diff total = (diff * correctionLength env) / total                 
  in
    case (player1Collision, player2Collision) of
      (True , False) -> Just $ correction (y - p1) (paddleHeight env)
      (False, True)  -> Just $ correction (y - p2) (paddleHeight env)
      (False, False) -> Nothing
      (True , True)  -> error "This can never happen" 

wallBounce :: Env -> PongGame -> PongGame
wallBounce env game = game { ballVel = (vx, vy') } 
  where 
    (vx, vy) = ballVel game

    vy' = if wallCollision env (ballLoc game) (ballRadius env)
            then - vy
            else  vy

wallCollision :: Env -> Position -> Radius -> Bool 
wallCollision env (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral (windowHeight env) / 2
    bottomCollision = y + radius >=  fromIntegral (windowHeight env) / 2  

movePaddle1 :: Env -> PongGame -> PongGame
movePaddle1 env game = 
  let (up, down) = (player1UpPressed game, player1DownPressed game)
  in case (up, down) of
    (True, _)    -> let p1' = player1 game + paddleStep env
                    in if p1' + paddleHeight env / 2 <= fromIntegral (windowHeight env) / 2 - wallHeight env
                        then game { player1 = p1' }
                        else game 
    (_   , True) -> let p1' = player1 game - paddleStep env
                    in if p1' - paddleHeight env / 2 >= negate (fromIntegral (windowHeight env) / 2) + wallHeight env
                        then game { player1 = p1' }
                        else game  
    (_, _)       -> game 

movePaddle2 :: Env -> PongGame -> PongGame
movePaddle2 env game = 
  let (up, down) = (player2UpPressed game, player2DownPressed game)
  in case (up, down) of
    (True, _)    -> let p2' = player2 game + paddleStep env
                    in if p2' + paddleHeight env / 2 <= fromIntegral (windowHeight env) / 2 - wallHeight env
                        then game { player2 = p2' }
                        else game 
    (_   , True) -> let p2' = player2 game - paddleStep env
                    in if p2' - paddleHeight env / 2 >= negate (fromIntegral (windowHeight env) / 2) + wallHeight env
                        then game { player2 = p2' }
                        else game  
    (_, _)       -> game 

checkPlayer2Pos :: PongGame -> PongGame
checkPlayer2Pos game = 
  let (_, y) = ballLoc game 
      p2 = player2 game
  in case p2 `compare` y of 
    EQ -> game { player2DownPressed = False, player2UpPressed = False }  
    GT -> game { player2DownPressed = True,  player2UpPressed = False }  
    LT -> game { player2DownPressed = False, player2UpPressed = True  }

checkGameEnded :: Env -> PongGame -> PongGame
checkGameEnded env game = 
  let (x, _) = ballLoc game
  in case (x > fromIntegral (windowWidth env) / 2, x < negate (fromIntegral (windowWidth env) / 2)) of 
    (True, _)    -> error "Player 2 wins"
    (_   , True) -> error "Player 1 wins"
    (_   , _   ) -> game
