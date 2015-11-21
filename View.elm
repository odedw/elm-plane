module View where
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug exposing (watch)
import Array
import Text
import List
import Random exposing (int, generate, initialSeed, Generator, Seed)
import Model exposing (..)
import Update exposing (..)

pillarToForm : Pillar -> Form
pillarToForm p =
  let
    imageName =
      if p.kind == Top then "/images/topRock.png"
      else "/images/bottomRock.png"
  in
    image constants.pillarWidth p.height imageName |>
    toForm |>
    move (p.x, p.y)

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
      ,  toForm (image constants.planeWidth constants.planeHeight "/images/plane.gif")
          |> move (constants.playerX, game.y)
      ,  toForm (image 400 70 "/images/textGameOver.png")
          |> alpha gameOverAlpha
      ,  toForm (image 400 70 "/images/textGetReady.png")
              |> alpha getReadyAlpha
      ]
    textLineStyle = (solid black)
    score =
        Text.fromString (toString game.score)
        |> (Text.height 50)
        |> Text.color yellow
        |> Text.bold
        |> outlinedText textLineStyle
        |> move (0, gameHeight/2 - 70)


    fullFormList =
      List.append formList
      <| Array.toList
      <| Array.push score pillarForms

  in
    container w h middle <|
    collage gameWidth gameHeight <|
    fullFormList
