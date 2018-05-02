module Fish.Fitting exposing (createPicture, createStylePicture)

import Fish.Vector exposing (Vector, add, scale, length)
import Fish.Box exposing (Box)
import Fish.Shape exposing (..)
import Fish.Style exposing (..)
import Fish.Picture exposing (Picture)

mapper : Box -> Vector -> Vector
mapper { a, b, c } { x, y } =
  add a (add (scale x b) (scale y c))

getStrokeWidth : Box -> Float
getStrokeWidth { b, c } =
  let
    s = max (length b) (length c)
  in
    s / 80.0

getStyle : Box -> Style
getStyle box =
  let sw = getStrokeWidth box in
  { stroke = Just { strokeWidth = sw
                  , strokeColor = "Black" }
  , fill = Nothing }

getDetailedStyle : String -> String -> Box -> Style
getDetailedStyle styling colorName box =
  let
    sw = getStrokeWidth box
    colorChoice = if colorName == "primary" then "Black" else "White"
    fillStyle = if styling == "fill" then Just { fillColor = colorChoice } else Nothing
    strokeStyle = if styling == "stroke" then Just { strokeWidth = sw, strokeColor = colorChoice } else Nothing
  in
    { stroke = strokeStyle, fill = fillStyle }

mapBezier : (Vector -> Vector) -> BezierShape -> BezierShape
mapBezier mirror { controlPoint1, controlPoint2, endPoint } =
  { controlPoint1 = mirror controlPoint1
  , controlPoint2 = mirror controlPoint2
  , endPoint = mirror endPoint }

mapShape : (Vector -> Vector) -> Shape -> Shape
mapShape m shape =
  case shape of
    Polygon { points } -> Polygon { points = List.map m points }

    Polyline { pts } -> Polyline { pts = List.map m pts }

    Curve { point1, point2, point3, point4 } ->
      Curve { point1 = m point1
            , point2 = m point2
            , point3 = m point3
            , point4 = m point4 }

    Path (vector, beziers) ->
      Path (m vector, beziers |> List.map (mapBezier m))

    x -> x

createStylePicture : List (String, String, Shape) -> Picture
createStylePicture styleShapes box =
  let
    m = mapper box
    handleStyleShape (styling, colorName, shape) =
      let
        mappedShape = mapShape m shape
        style = getDetailedStyle styling colorName box
      in
        (mappedShape, style)
  in
    styleShapes |> List.map handleStyleShape

createPicture : List Shape -> Picture
createPicture shapes box =
  let
    m = mapper box
    style = getStyle box
  in
    shapes |> List.map (mapShape m) |> List.map (\s -> (s, style))
