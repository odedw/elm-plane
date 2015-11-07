import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug

-- MODEL

(gameWidth,gameHeight) = (800,480)

type State = Play | Starting | GameOver

constants =
  { backgroundScrollV = 40
  , foregroundScrollV = 80
  , playerX = 100 - gameWidth / 2
  }

type alias Game =
  { state : State
  , foregroundX : Float
  , backgroundX : Float
  , playerY : Float
  }

defaultGame : Game
defaultGame =
  { state = Starting
  , foregroundX = 0
  , backgroundX = 0
  , playerY = 0
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
    newPlayerY =
      updatePlayerY input game
    newBackgroundX =
      updateBackground input.delta game.backgroundX
  in
    {newGame |
        -- foregroundX <- game.foregroundX + input.delta * constants.backgroundScrollV
        backgroundX <- newBackgroundX
    ,   playerY <- newPlayerY
    }

updatePlayerY : Input -> Game -> Float
updatePlayerY input game =
  case game.state of
    Starting -> game.playerY + (sin (game.backgroundX / 10))
    GameOver -> game.playerY
    Play     -> game.playerY


updateBackground : Time -> Float -> Float
updateBackground delta currentBackgroundX =
  if currentBackgroundX > gameWidth
    then 0
  else
    currentBackgroundX + delta * constants.backgroundScrollV

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
      Signal.map inSeconds (fps 35)


input : Signal Input
input =
        Signal.sampleOn delta <|
          Signal.map2 Input
          Keyboard.space
          delta
