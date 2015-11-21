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
        |> Text.color (Color.rgb 50 160 50)
        |> Text.bold
    scoreForm =
      score
      |> text
      |> move (0, gameHeight/2 - 70)
    scoreOutlineForm =
      score
      |> outlinedText (solid (Color.rgb 0 80 0))
      |> move (0, gameHeight/2 - 70)

    textForms =
      if | game.state == GameOver -> [ toForm (image 250 45 "/images/textGameOver.png")
                                       |> move (0,70)
                                     , scoreOutlineForm
                                     , scoreForm
                                     ]
         | game.state == Start -> [ toForm (image 250 45 "/images/textGetReady.png")
                                    |> move (0,70)
                                  , scoreOutlineForm
                                  , scoreForm
                                  ]
         | otherwise -> [ scoreOutlineForm
                        , scoreForm
                        ]

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
