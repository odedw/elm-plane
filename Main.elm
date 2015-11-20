import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug exposing (watch)
import Array
import List
import Random exposing (int, generate, initialSeed, Generator, Seed)

(gameWidth,gameHeight) = (800,480)

type State = Play | Start | GameOver
type Kind = Top | Bottom
type alias Column =
  { x : Float
  , columnHeight: Int
  , kind : Kind
  }
type alias Constants =
  {
  backgroundScrollV : Float
  , foregroundScrollV : Float
  , playerX : Float
  , jumpSpeed : Float
  , gravity : Float
  , timeBetweenColumns : Float
  , columnWidth : Int
  , minColumnHeight : Int
  , planeHeight : Int
  , gapToPlaneRatio : Float
  , gapHeight : Int
  }
type alias Game =
  { state : State
  , foregroundX : Float
  , backgroundX : Float
  , y : Float
  , vy : Float
  , timeToColumn : Float
  , columns : Array.Array Column
  , randomizer : Generator Int
  }

type alias KeyUpdate =
  Bool -> Game -> Game

type alias TimeUpdate =
  (Time,Time) -> Game -> Game

constants : Constants
constants =
  let
    planeHeight = 35
    gapToPlaneRatio = 4
    gapHeight = round ((toFloat planeHeight) * gapToPlaneRatio)
  in
    {
    backgroundScrollV = 40
    , foregroundScrollV = 150
    , playerX = 100 - gameWidth / 2
    , jumpSpeed = 370.0
    , gravity = 1500.0
    , timeBetweenColumns = 1.8
    , columnWidth = 30
    , minColumnHeight = round (gameHeight / 8)
    , planeHeight = planeHeight
    , gapToPlaneRatio = gapToPlaneRatio
    , gapHeight = gapHeight
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
  , randomizer = Random.int constants.minColumnHeight (gameHeight - constants.minColumnHeight - constants.gapHeight)
  }

-- UPDATE
update : Input -> Game -> Game
update input game =
  -- let
    -- newGame = Debug.watch "game" game
  -- in
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
       | game.state == Play -> game.y + game.vy * (snd delta)
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
       | otherwise -> game.backgroundX + (snd delta) * constants.backgroundScrollV
  }

applyPhysics : TimeUpdate
applyPhysics delta game =
  {game | vy <-
    if | game.state == GameOver -> 0
       | otherwise -> game.vy - (snd delta) * constants.gravity
  }

updateColumns : TimeUpdate
updateColumns delta game =
  let
    timeToColumn =
      if | game.timeToColumn <= 0 -> constants.timeBetweenColumns
         | game.state == Play -> game.timeToColumn - (snd delta)
         | otherwise -> game.timeToColumn
    shouldAddColumn = timeToColumn == constants.timeBetweenColumns && game.state == Play
    updatedColumns =
      Array.map (\c -> {c | x <- c.x - constants.foregroundScrollV * (snd delta)}) game.columns
    columns =
      if | game.state /= Play -> game.columns
         | shouldAddColumn -> Array.append  (generateColumns (fst delta) game) updatedColumns
         | otherwise -> updatedColumns

  in
    {game | timeToColumn <- timeToColumn
          , columns <- columns
    }

generateColumns : Time -> Game -> Array.Array Column
generateColumns time game =
  let
    bottomHeight =
      fst <| generate game.randomizer <| initialSeed <| round <| inMilliseconds time
    topHeight =
      gameHeight - bottomHeight - constants.gapHeight
  in
    Array.fromList <|
    [
      {
      x = gameWidth / 2 + (toFloat constants.columnWidth)
      , columnHeight = bottomHeight
      , kind = Bottom
      }
      ,
      {
      x = gameWidth / 2 + (toFloat constants.columnWidth)
      , columnHeight = topHeight
      , kind = Top
      }
    ]

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
columnToForm : Column -> Form
columnToForm c =
  let
    imageName =
      if c.kind == Top then "/images/topRock.png"
      else "/images/bottomRock.png"
    y =
      if c.kind == Top then (gameHeight/2 - (toFloat c.columnHeight/2))
      else (toFloat c.columnHeight/2) - (gameHeight/2)
  in
    image constants.columnWidth c.columnHeight imageName |>
    toForm |>
    move (c.x, y)

view : (Int,Int) -> Game -> Element
view (w,h) game =
  let
    gameOverAlpha =
      if game.state == GameOver then 1 else 0
    getReadyAlpha =
      if game.state == Start then 1 else 0
    columnForms =
      Array.map columnToForm game.columns
    formList =
      [
         toForm (image gameWidth gameHeight "/images/background.png")
           |> move (-game.backgroundX, 0)
      ,  toForm (image gameWidth gameHeight "/images/background.png")
           |> move (gameWidth - game.backgroundX, 0)
      ,  toForm (image 60 constants.planeHeight "/images/plane.gif")
          |> move (constants.playerX, game.y)
      ,  toForm (image 400 70 "/images/textGameOver.png")
          |> alpha gameOverAlpha
      ,  toForm (image 400 70 "/images/textGetReady.png")
              |> alpha getReadyAlpha
      ]
  in
    container w h middle <|
    collage gameWidth gameHeight <|
    List.append formList (Array.toList columnForms)

-- SIGNALS
type Input =
    TimeDelta (Time,Time)
  | Space Bool

main =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
    Signal.foldp update defaultGame input

delta = timestamp <|
      Signal.map inSeconds (fps 45)

input : Signal Input
input =
        Signal.mergeMany [Signal.map TimeDelta delta, Signal.map Space Keyboard.space]
