module Utils where
import Debug exposing (watch)
import Types exposing (..)

isColliding : Constants -> Game -> Pillar -> Bool
isColliding constants game pillar =
  let
    r1 =
      {left = constants.playerX - (toFloat constants.planeWidth)/2 + constants.epsilon
      , top = game.y + (toFloat constants.planeHeight)/2
      , right = constants.playerX + (toFloat constants.planeWidth)/2 - constants.epsilon
      , bottom = game.y - (toFloat constants.planeHeight)/2
      }
    r2 =
      {left = pillar.x - (toFloat constants.pillarWidth)/2 + constants.epsilon
      , top = pillar.y + (toFloat pillar.height)/2
      , right = pillar.x + (toFloat constants.pillarWidth)/2 - constants.epsilon
      , bottom = pillar.y - (toFloat pillar.height)/2
      }
  in
     not
        (r2.left > r1.right ||
         r2.right < r1.left ||
         r2.top < r1.bottom ||
         r2.bottom > r1.top)
