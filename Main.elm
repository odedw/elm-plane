import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug
import Array
import List

(gameWidth,gameHeight) = (800,480)

type State = Play | Start | GameOver

type alias Column =
  { x : Float
  , bottomHeight: Float
  , topHeight: Float
  }
type alias Game =
  { state : State
  , foregroundX : Float
  , backgroundX : Float
  , y : Float
  , vy : Float
  , timeToColumn : Float
  , columns : Array.Array Column
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
  , timeBetweenColumns = 2
  }

-- MODEL
defaultGame : Game
defaultGame =
  { state = Start
  , foregroundX = 0
  , backgroundX = 0
  , y = 0
  , vy = 0
  , timeToColumn = constants.timeBetweenColumns
  , columns = Array.empty
  }

-- UPDATE
update : Input -> Game -> Game
update input game =
  let
    newGame = Debug.watch "game" game
  in
    case input of
      TimeDelta delta ->
        --TODO: can I pass the delta somehow as well?
        game
          |> updatePlayerY delta
          |> updateBackground delta
          |> applyPhysics delta
          |> checkFailState delta
          |> updateColumns delta
      Space space ->
        game
          |> transitionState space
          |> updatePlayerVelocity space

--Time updates
updatePlayerY : TimeUpdate
updatePlayerY delta game =
  {game | y <-
    if | game.state == Start -> game.y + (sin (game.backgroundX / 10))
       | game.state == Play -> game.y + game.vy * delta
       | otherwise -> game.y
  }

checkFailState : TimeUpdate
checkFailState delta game =
  {game | state <-
    if game.state == Play && game.y <= -gameHeight/2 then GameOver
    else game.state
  }

updateBackground : TimeUpdate
updateBackground delta game =
  {game | backgroundX <-
    if | game.backgroundX > gameWidth -> 0
       | game.state == GameOver -> game.backgroundX
       | otherwise -> game.backgroundX + delta * constants.backgroundScrollV
  }

applyPhysics : TimeUpdate
applyPhysics delta game =
  {game | vy <-
    if | game.state == GameOver -> 0
       | otherwise -> game.vy - delta * constants.gravity
  }

updateColumns : TimeUpdate
updateColumns delta game =
  let
    timeToColumn =
      if | game.timeToColumn <= 0 -> constants.timeBetweenColumns
         | game.state == Play -> game.timeToColumn - delta
         | otherwise -> game.timeToColumn
    shouldAddColumn = timeToColumn == constants.timeBetweenColumns && game.state == Play
    updatedColumns =
      Array.map (\c -> {c | x <- c.x - constants.foregroundScrollV * delta}) game.columns
    columns =
      if | game.state /= Play -> game.columns
         | shouldAddColumn ->
            updatedColumns |>
              Array.push {
                x = gameWidth
                , bottomHeight = 0
                , topHeight = 0
              }
         | otherwise -> updatedColumns
  in
    {game | timeToColumn <- timeToColumn
          , columns <- columns
    }

--Input updates
transitionState : KeyUpdate
transitionState space game =
  if game.state == GameOver && space then defaultGame --Reset
  else
    {game |
      state <-
        if | game.state == Start && space -> Play
           | otherwise -> game.state
    }

updatePlayerVelocity : KeyUpdate
updatePlayerVelocity space game =
  {game | vy <-
    if space then constants.jumpSpeed
    else game.vy
  }

-- VIEW
view : (Int,Int) -> Game -> Element
view (w,h) game =
  let
    gameOverAlpha =
      if game.state == GameOver then 1 else 0
    getReadyAlpha =
      if game.state == Start then 1 else 0
    columnForms =
      Array.map (\c -> toForm(image 108 239 "/images/rock.png") |> move (c.x, 0)) game.columns
    formList =
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
  in
    container w h middle <|
    collage gameWidth gameHeight formList
      -- (List.append (Array.toList columnForms) formList)

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
