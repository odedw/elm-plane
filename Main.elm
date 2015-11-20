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
type alias Pillar =
  { x : Float
  , pillarHeight: Int
  , kind : Kind
  }
type alias Constants =
  {
  backgroundScrollV : Float
  , foregroundScrollV : Float
  , playerX : Float
  , jumpSpeed : Float
  , gravity : Float
  , timeBetweenPillars : Float
  , pillarWidth : Int
  , minPillarHeight : Int
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
  , timeToPillar : Float
  , pillars : Array.Array Pillar
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
    , timeBetweenPillars = 1.8
    , pillarWidth = 30
    , minPillarHeight = round (gameHeight / 8)
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
  , timeToPillar = constants.timeBetweenPillars
  , pillars = Array.empty
  , randomizer = Random.int constants.minPillarHeight (gameHeight - constants.minPillarHeight - constants.gapHeight)
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
          |> updatePillars delta
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

updatePillars : TimeUpdate
updatePillars delta game =
  let
    timeToPillar =
      if | game.timeToPillar <= 0 -> constants.timeBetweenPillars
         | game.state == Play -> game.timeToPillar - (snd delta)
         | otherwise -> game.timeToPillar
    shouldAddPillar = timeToPillar == constants.timeBetweenPillars && game.state == Play
    updatedPillars =
      Array.map (\c -> {c | x <- c.x - constants.foregroundScrollV * (snd delta)}) game.pillars
    pillars =
      if | game.state /= Play -> game.pillars
         | shouldAddPillar -> Array.append  (generatePillars (fst delta) game) updatedPillars
         | otherwise -> updatedPillars

  in
    {game | timeToPillar <- timeToPillar
          , pillars <- pillars
    }

generatePillars : Time -> Game -> Array.Array Pillar
generatePillars time game =
  let
    bottomHeight =
      fst <| generate game.randomizer <| initialSeed <| round <| inMilliseconds time
    topHeight =
      gameHeight - bottomHeight - constants.gapHeight
  in
    Array.fromList <|
    [
      {
      x = gameWidth / 2 + (toFloat constants.pillarWidth)
      , pillarHeight = bottomHeight
      , kind = Bottom
      }
      ,
      {
      x = gameWidth / 2 + (toFloat constants.pillarWidth)
      , pillarHeight = topHeight
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
pillarToForm : Pillar -> Form
pillarToForm c =
  let
    imageName =
      if c.kind == Top then "/images/topRock.png"
      else "/images/bottomRock.png"
    y =
      if c.kind == Top then (gameHeight/2 - (toFloat c.pillarHeight/2))
      else (toFloat c.pillarHeight/2) - (gameHeight/2)
  in
    image constants.pillarWidth c.pillarHeight imageName |>
    toForm |>
    move (c.x, y)

view : (Int,Int) -> Game -> Element
view (w,h) game =
  let
    gameOverAlpha =
      if game.state == GameOver then 1 else 0
    getReadyAlpha =
      if game.state == Start then 1 else 0
    pillarForms =
      Array.map pillarToForm game.pillars
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
    List.append formList (Array.toList pillarForms)

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
