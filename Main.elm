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

type alias Input =
    { space : Bool
    , delta : Time
    }

-- UPDATE
update : Input -> Game -> Game
update  input game =
  let
    newGame =
      Debug.watch "game" game
    debugInput =
      Debug.watch "input" input
    newPlayerY =
      updatePlayerY input game
    newBackgroundX =
        updateBackground input game
    newState =
      updateState input game
    newVY =
      updatePlayerVelocity input game
  in
    {newGame |
        -- foregroundX <- game.foregroundX + input.delta * constants.backgroundScrollV
        backgroundX <- newBackgroundX
    ,   playerY     <- newPlayerY
    ,   state       <- newState
    ,   playerVY    <- newVY
    }

updatePlayerY : Input -> Game -> Float
updatePlayerY input game =
  if | game.state == Start -> game.playerY + (sin (game.backgroundX / 10))
     | game.state == GameOver && input.space -> 0
     | game.state == Play -> game.playerY + game.playerVY * input.delta
     | otherwise -> game.playerY

updatePlayerVelocity : Input -> Game -> Float
updatePlayerVelocity input game =
  if | game.state == GameOver -> 0
     | input.space -> constants.jumpSpeed
     | otherwise -> game.playerVY - input.delta * constants.gravity

updateState : Input -> Game -> State
updateState input game =
  if | game.state == Start && input.space -> Play
     | game.state == GameOver && input.space -> Start
     | game.state == Play && game.playerY <= 20-gameHeight/2 -> GameOver
     | otherwise -> game.state

updateBackground : Input -> Game -> Float
updateBackground input game =
  if | game.backgroundX > gameWidth -> 0
     | game.state == GameOver -> game.backgroundX
     | otherwise -> game.backgroundX + input.delta * constants.backgroundScrollV

-- VIEW
view : (Int,Int) -> Game -> Element
view (w,h) {state, foregroundX, backgroundX, playerY} =
  -- let
  -- in
    container w h middle <|
    collage gameWidth gameHeight
     [
        toForm (image gameWidth gameHeight "/images/background.png")
          |> move (-backgroundX, 0)
     ,  toForm (image gameWidth gameHeight "/images/background.png")
          |> move (gameWidth - backgroundX, 0)
     ,  toForm (image 60 35 "/images/plane.gif")
         |> move (constants.playerX, playerY)
     ]

-- SIGNALS

main =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
    Signal.foldp update defaultGame input

delta =
      Signal.map inSeconds (fps 60)


input : Signal Input
input =
        Signal.sampleOn delta <|
          Signal.map2 Input
          Keyboard.space
          delta
