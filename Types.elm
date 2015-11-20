module Types where
import Array
import Random exposing (int, generate, initialSeed, Generator, Seed)
import Time exposing (..)

type State = Play | Start | GameOver
type Kind = Top | Bottom
type alias Pillar =
  { x : Float
  , y : Float
  , height: Int
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
  , planeWidth : Int
  , gapToPlaneRatio : Float
  , gapHeight : Int
  , epsilon : Float
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

type Input =
      TimeDelta (Time,Time)
    | Space Bool
