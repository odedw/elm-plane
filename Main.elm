import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug

-- MODEL

(gameWidth,gameHeight) = (800,480)

type State = Play | Start | GameOver

constants =
  { backgroundScrollV = 40
  , foregroundScrollV = 80
  , playerX = 100 - gameWidth / 2
  , jumpSpeed = 350.0
  , gravity = 1500.0
  }

type alias Game =
  { state : State
  , foregroundX : Float
  , backgroundX : Float
  , playerY : Float
  , playerVY : Float
  }

defaultGame : Game
defaultGame =
  { state = Start
  , foregroundX = 0
  , backgroundX = 0
  , playerY = 0
  , playerVY = 0
  }

-- UPDATE
update : Input -> Game -> Game
update input game =
  let
    newGame =
      Debug.watch "game" game
  in
    case input of
      TimeDelta delta ->
        {game |
          playerY     <- updatePlayerY delta game
        , backgroundX <- updateBackground delta game
        , playerVY    <- applyPhysics delta game
        , state       <- checkFailState delta game
        }
      Space space ->
        {game |
          state     <-  transitionState space game
        , playerVY  <-  updatePlayerVelocity space game
        }

--Time updates
updatePlayerY : Time -> Game -> Float
updatePlayerY delta game =
  if | game.state == Start -> game.playerY + (sin (game.backgroundX / 10))
     | game.state == Play -> game.playerY + game.playerVY * delta
     | otherwise -> game.playerY

checkFailState : Time -> Game -> State
checkFailState delta game =
  if game.state == Play && game.playerY <= -gameHeight/2 then GameOver
  else game.state

updateBackground : Time -> Game -> Float
updateBackground delta game =
  if | game.backgroundX > gameWidth -> 0
     | game.state == GameOver -> game.backgroundX
     | otherwise -> game.backgroundX + delta * constants.backgroundScrollV

applyPhysics : Time -> Game -> Float
applyPhysics delta game =
  if | game.state == GameOver -> 0
     | otherwise -> game.playerVY - delta * constants.gravity

--Input updates
transitionState : Bool -> Game -> State
transitionState space game =
      if | game.state == Start && space -> Play
         | game.state == GameOver && space -> Start
         | otherwise -> game.state

updatePlayerVelocity : Bool -> Game -> Float
updatePlayerVelocity space game =
  if space then constants.jumpSpeed
  else game.playerVY

-- VIEW
view : (Int,Int) -> Game -> Element
view (w,h) game =
  let
    gameOverAlpha =
      if game.state == GameOver then 1 else 0
    getReadyAlpha =
      if game.state == Start then 1 else 0
  in
    container w h middle <|
    collage gameWidth gameHeight
     [
        toForm (image gameWidth gameHeight "/images/background.png")
          |> move (-game.backgroundX, 0)
     ,  toForm (image gameWidth gameHeight "/images/background.png")
          |> move (gameWidth - game.backgroundX, 0)
     ,  toForm (image 60 35 "/images/plane.gif")
         |> move (constants.playerX, game.playerY)
     ,  toForm (image 400 70 "/images/textGameOver.png")
         |> alpha gameOverAlpha
     ,  toForm (image 400 70 "/images/textGetReady.png")
             |> alpha getReadyAlpha
     ]

-- SIGNALS
type Input =
    TimeDelta Time
  | Space Bool

main =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
    Signal.foldp update defaultGame input


delta =
      Signal.map inSeconds (fps 60)



input : Signal Input
input =
        Signal.mergeMany [Signal.map TimeDelta delta, Signal.map Space Keyboard.space]
