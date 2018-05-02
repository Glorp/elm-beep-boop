module Fish.Boxy exposing (boxThing)

import Fish.Vector exposing (Vector)
import Fish.Shape exposing (..)

createVector : Float -> Float -> Vector
createVector x y = { x = x, y = y }

boxVectors : List Vector
boxVectors = [
    createVector 0.0 0.0
  , createVector 0.0 1.0
  , createVector 1.0 1.0
  , createVector 1.0 0.0 ]

boxShape : PolygonShape
boxShape =
  { points = boxVectors }

boxThing : List (String, String, Shape)
boxThing =
  [ ("fill", "primary", Polygon boxShape) ]
