import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug

-- MODEL

(gameWidth,gameHeight) = (800,480)

type State = Play | Pause
constants =
  { backgroundScrollV = 40
  , foregroundScrollV = 80
  }

type alias Game =
  { state : State
  , foregroundX : Float
  , backgroundX : Float
  }

defaultGame : Game
defaultGame =
  { state = Pause
  , foregroundX = 0
  , backgroundX = 0
  }

type alias Input =
    { space : Bool
    , delta : Time
    }

-- UPDATE
update : Input -> Game -> Game
update  {space,delta} ({state,foregroundX,backgroundX} as game) =
  let
    newGame =
      Debug.watch "game" game
  in
    {newGame |
      foregroundX <- game.foregroundX + delta * constants.backgroundScrollV,
      backgroundX <-
        if game.backgroundX > gameWidth
        then
          0
        else
          game.backgroundX + delta * constants.backgroundScrollV
    }

-- VIEW
view : (Int,Int) -> Game -> Element
view (w,h) {state, foregroundX, backgroundX} =
  -- let
  -- in
    container w h middle <|
    collage gameWidth gameHeight
     [


        toForm (image gameWidth gameHeight "/images/background.png")
          |> move (-backgroundX, 0)
     ,  toForm (image gameWidth gameHeight "/images/background.png")
          |> move (gameWidth - backgroundX, 0)
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
