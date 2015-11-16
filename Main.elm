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

type alias Game =
  { state : State
  , foregroundX : Float
  , backgroundX : Float
  , y : Float
  , vy : Float
  }

type alias KeyUpdate =
  Bool -> Game -> Game

type alias TimeUpdate =
  Time -> Game -> Game
constants =
  { backgroundScrollV = 40
  , foregroundScrollV = 80
  , playerX = 100 - gameWidth / 2
  , jumpSpeed = 350.0
  , gravity = 1500.0
  }


defaultGame : Game
defaultGame =
  { state = Start
  , foregroundX = 0
  , backgroundX = 0
  , y = 0
  , vy = 0
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
        --TODO: can I pass the delta somehow as well?
        game
          |> updatePlayerY delta
          |> updateBackground delta
          |> applyPhysics delta
          |> checkFailState delta
      Space space ->
        game
          |> transitionState space
          |> updatePlayerVelocity space

--Time updates
updatePlayerY : TimeUpdate
updatePlayerY delta game =
  let
    y =
      if | game.state == Start -> game.y + (sin (game.backgroundX / 10))
         | game.state == Play -> game.y + game.vy * delta
         | otherwise -> game.y
  in
    {game | y <- y}

checkFailState : TimeUpdate
checkFailState delta game =
  let
    state =
      if game.state == Play && game.y <= -gameHeight/2 then GameOver
      else game.state
  in
    {game | state <- state}

updateBackground : TimeUpdate
updateBackground delta game =
  let
    bx =
      if | game.backgroundX > gameWidth -> 0
         | game.state == GameOver -> game.backgroundX
         | otherwise -> game.backgroundX + delta * constants.backgroundScrollV
  in
    {game | backgroundX <- bx}

applyPhysics : TimeUpdate
applyPhysics delta game =
  let
    vy =
      if | game.state == GameOver -> 0
         | otherwise -> game.vy - delta * constants.gravity
  in
    {game | vy <- vy}

--Input updates
transitionState : KeyUpdate
transitionState space game =
  let
    state =
      if | game.state == Start && space -> Play
         | game.state == GameOver && space -> Start
         | otherwise -> game.state
    y =
      if game.state == GameOver && state == Start then 0
      else game.y
  in
    {game |
      state <- state
    , y     <- y
    }

updatePlayerVelocity : KeyUpdate
updatePlayerVelocity space game =
  let
    vy =
      if space then constants.jumpSpeed
      else game.vy
  in
    {game | vy <- vy}

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
         |> move (constants.playerX, game.y)
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
      Signal.map inSeconds (fps 45)

input : Signal Input
input =
        Signal.mergeMany [Signal.map TimeDelta delta, Signal.map Space Keyboard.space]
