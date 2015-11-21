module View where
import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug exposing (watch)
import Array
import Text
import List
import Model exposing (..)
import Update exposing (..)

pillarToForm : Pillar -> Form
pillarToForm p =
  let
    imageName =
      if p.kind == Top then
        "/images/topRock.png"
      else
        "/images/bottomRock.png"
  in
    image constants.pillarWidth p.height imageName
    |> toForm
    |> move (p.x, p.y)

view : (Int,Int) -> Game -> Element
view (w,h) game =
  let
    pillarForms =
      Array.map pillarToForm game.pillars
      |> Array.toList

    backgroundForms =
      [ toForm (image gameWidth gameHeight "/images/background.png")
        |> move (-game.backgroundX, 0)
      , toForm (image gameWidth gameHeight "/images/background.png")
        |> move (gameWidth - game.backgroundX, 0)
      ]
    score =
        Text.fromString (toString game.score)
        |> (Text.height 50)
        |> Text.color Color.yellow
        |> Text.bold
        |> outlinedText (solid Color.black)
        |> move (0, gameHeight/2 - 70)

    textForms =
      if | game.state == GameOver -> [ toForm (image 400 70 "/images/textGameOver.png")
                                     , score
                                     ]
         | game.state == Start -> [ toForm (image 400 70 "/images/textGetReady.png")
                                  , score
                                  ]
         | otherwise -> [score]

    playerForm =
      [ toForm (image constants.planeWidth constants.planeHeight "/images/plane.gif")
        |> move (constants.playerX, game.y)
      ]

    fullFormList =
      List.append backgroundForms
      <| List.append playerForm
      <| List.append pillarForms
      <| textForms
  in
    container w h middle
    <| collage gameWidth gameHeight
    <| fullFormList
