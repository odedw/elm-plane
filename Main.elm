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
import View exposing (..)

main =
  Signal.map2 view Window.dimensions gameState

gameState : Signal Game
gameState =
    Signal.foldp update defaultGame input

delta =
  timestamp
  <| Signal.map inSeconds (fps 45)

input : Signal Input
input =
        Signal.mergeMany [Signal.map TimeDelta delta, Signal.map Space Keyboard.space]
