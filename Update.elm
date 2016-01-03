module Update where
import Keyboard
import Time exposing (..)
import Debug exposing (watch)
import Array
import Random exposing (int, generate, initialSeed, Generator, Seed)
import Model exposing (..)

update : Input -> Game -> Game
update input game =
  case input of
    TimeDelta delta ->
      game
        |> updatePlayerY delta
        |> updateBackground delta
        |> applyPhysics delta
        |> checkFailState delta
        |> updatePillars delta
        |> updateScore delta

    Space space ->
      game
        |> transitionState space
        |> updatePlayerVelocity space

--Time updates
updatePlayerY : TimeUpdate
updatePlayerY delta game =
  {game | y =
    if game.state == Start then
      game.y + (sin (game.backgroundX / 10))
    else if game.state == Play || (game.state == GameOver && game.y > -gameHeight/2) then
      game.y + game.vy * (snd delta)
    else
      game.y
  }

isColliding : Constants -> Game -> Pillar -> Bool
isColliding constants game pillar =
  let
    r1 =
      { left = constants.playerX - (toFloat constants.planeWidth)/2 + constants.epsilon
      , top = game.y + (toFloat constants.planeHeight)/2
      , right = constants.playerX + (toFloat constants.planeWidth)/2 - constants.epsilon
      , bottom = game.y - (toFloat constants.planeHeight)/2
      }
    r2 =
      { left = pillar.x - (toFloat constants.pillarWidth)/2 + constants.epsilon
      , top = pillar.y + (toFloat pillar.height)/2
      , right = pillar.x + (toFloat constants.pillarWidth)/2 - constants.epsilon
      , bottom = pillar.y - (toFloat pillar.height)/2
      }
  in
     not
     <| r2.left > r1.right
        || r2.right < r1.left
        || r2.top < r1.bottom
        || r2.bottom > r1.top

checkFailState : TimeUpdate
checkFailState delta game =
  let
    playerOffScreen =
      game.y <= -gameHeight/2
    collisionPillars =
      Array.filter (\p -> isColliding constants game p) game.pillars
      |> Array.length
    playerCollidedWithPillar =
      collisionPillars > 0
  in
    {game | state =
      if game.state == Play && (playerOffScreen || playerCollidedWithPillar) then
        GameOver
      else
        game.state
    }

updateBackground : TimeUpdate
updateBackground delta game =
  {game | backgroundX =
    if game.backgroundX > gameWidth then
      0
    else if game.state == GameOver then
      game.backgroundX
    else
      game.backgroundX + (snd delta) * constants.backgroundScrollV
  }

applyPhysics : TimeUpdate
applyPhysics delta game =
  {game | vy =
    if game.state == Play || game.state == GameOver && game.y > -gameHeight/2 then
      game.vy - (snd delta) * constants.gravity
    else
      0
  }

updatePillars : TimeUpdate
updatePillars delta game =
  let
    timeToPillar =
      if game.timeToPillar <= 0 then
        constants.timeBetweenPillars
      else if game.state == Play then
        game.timeToPillar - (snd delta)
      else
        game.timeToPillar
    shouldAddPillar = timeToPillar == constants.timeBetweenPillars && game.state == Play
    updatedPillars =
      Array.map (\p -> {p | x = p.x - constants.foregroundScrollV * (snd delta)}) game.pillars |>
      Array.filter (\p -> p.x > -(gameWidth/2))
    pillars =
      if game.state /= Play then
        game.pillars
      else if shouldAddPillar then
        Array.append (generatePillars (fst delta) game) updatedPillars
      else
        updatedPillars

  in
    { game | timeToPillar = timeToPillar
          , pillars = pillars
    }

generatePillars : Time -> Game -> Array.Array Pillar
generatePillars time game =
  let
    bottomHeight =
      fst <| generate constants.randomizer <| initialSeed <| round <| inMilliseconds time
    topHeight =
      gameHeight - bottomHeight - constants.gapHeight
  in
    Array.fromList <|
    [
      { x = gameWidth/2 + (toFloat constants.pillarWidth)
      , y = (toFloat bottomHeight/2) - (gameHeight/2)
      , height = bottomHeight
      , kind = Bottom
      , passed = False
      }
      ,
      { x = gameWidth/2 + (toFloat constants.pillarWidth)
      , y = (gameHeight/2 - (toFloat topHeight/2))
      , height = topHeight
      , kind = Top
      , passed = False
      }
    ]

updateScore : TimeUpdate
updateScore delta game =
  let
    length =
      Array.length <| Array.filter (\p -> not p.passed && p.x < constants.playerX) game.pillars
    pillars =
      if (length > 0) then
        Array.map (\p -> if not p.passed && p.x < constants.playerX then {p | passed = True} else p) game.pillars
      else
        game.pillars
  in
    {game |
      pillars = pillars
    , score =
      if length > 0 then
        game.score + 1
      else
        game.score
    }

--Input updates
transitionState : KeyUpdate
transitionState space game =
  if game.state == GameOver && game.y <= -gameHeight/2 && space then
    defaultGame --Reset
  else
    {game |
      state =
        if game.state == Start && space then
          Play
        else
          game.state
    }

updatePlayerVelocity : KeyUpdate
updatePlayerVelocity space game =
  {game | vy =
    if game.state == Play && space then
      constants.jumpSpeed
    else
      game.vy
  }
