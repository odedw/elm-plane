module Utils where
import Debug exposing (watch)
import Types exposing (..)

isColliding : Constants -> Game -> Pillar -> Bool
isColliding constants game pillar =
  let
    r1 =
      {left = constants.playerX - (toFloat constants.planeWidth)/2 + constants.epsilon
      , top = game.y + (toFloat constants.planeHeight)/2 - constants.epsilon
      , right = constants.playerX + (toFloat constants.planeWidth)/2 - constants.epsilon
      , bottom = game.y - (toFloat constants.planeHeight)/2 + constants.epsilon
      }
    r2 =
      {left = pillar.x - (toFloat constants.pillarWidth)/2 + constants.epsilon
      , top = pillar.y + (toFloat pillar.height)/2 - constants.epsilon
      , right = pillar.x + (toFloat constants.pillarWidth)/2 - constants.epsilon
      , bottom = pillar.y - (toFloat pillar.height)/2 + constants.epsilon
      }
    r1d = Debug.watch "r1" r1
    r2d = Debug.watch "r2" r2
  in
     not
        (r2.left > r1.right ||
         r2.right < r1.left ||
         r2.top < r1.bottom ||
         r2.bottom > r1.top)
