module Fish.Rendering exposing (toSvg)

import Fish.Vector exposing (Vector)
import Fish.Shape exposing (..)
import Fish.Style exposing (..)
import Fish.Picture exposing (..)
import Fish.Mirror exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

getStrokeWidthFromStyle : Maybe StrokeStyle -> Float
getStrokeWidthFromStyle style =
  case style of
    Just { strokeWidth } -> sqrt strokeWidth
    Nothing -> 2.0

toPolygonElement : Style -> List Vector -> Svg msg
toPolygonElement style pts =
  let
    s =
      let
        str {x, y} = (toString x) ++ "," ++ (toString y)
      in
        pts |> List.map str |> String.join " "
    sw = getStrokeWidthFromStyle style.stroke
    (strokeStr, fillStr) =
      case style.fill of
      Just { fillColor } ->
          ("None", fillColor)
      Nothing -> ("White", "None")
  in
    Svg.polygon
      [ stroke "White"
      , strokeWidth <| toString 2
      , fill fillStr
      , points s ] []

toPolylineElement : Style -> List Vector -> Svg msg
toPolylineElement style pts =
  let
    s =
      let
        str {x, y} = (toString x) ++ "," ++ (toString y)
      in
        pts |> List.map str |> String.join " "
    sw = getStrokeWidthFromStyle style.stroke
  in
    Svg.polyline
      [ stroke "Black"
      , strokeWidth <| toString sw
      , fill "None"
      , points s ] []

toCurveElement : Style -> Vector -> Vector -> Vector -> Vector -> Svg msg
toCurveElement style pt1 pt2 pt3 pt4 =
  let
    toStr {x, y} = (toString x) ++ " " ++ (toString y)
    pt1s = toStr pt1
    pt2s = toStr pt2
    pt3s = toStr pt3
    pt4s = toStr pt4
    dval = "M" ++ pt1s ++ " C " ++ pt2s ++ ", " ++ pt3s ++ ", " ++ pt4s
    sw = getStrokeWidthFromStyle style.stroke
  in
    Svg.path
      [ stroke "White"
      , strokeWidth <| toString sw
      , fill "None"
      , d dval ] []

toPathElement : Style -> Vector -> List BezierShape -> Svg msg
toPathElement style v beziers =
  -- sprintf "C %f %f, %f %f, %f %f" x1 y1 x2 y2 x3 y3
  -- startStr = sprintf "M%f %f" x y
  let
    toStr { x, y } = (toString x) ++ " " ++ (toString y)
    toControl { controlPoint1
              , controlPoint2
              , endPoint } =
      " C " ++ (toStr controlPoint1) ++ ", " ++ (toStr controlPoint2) ++ ", " ++ (toStr endPoint)
    startPt = toStr v
    controlPts = beziers |> List.map toControl |> List.foldr (++) ""
    dval = "M" ++ startPt ++ controlPts
    sw = getStrokeWidthFromStyle style.stroke
    (strokeStr, fillStr) =
      case style.fill of
      Just { fillColor } ->
          ("None", fillColor)
      Nothing -> ("White", "None")
  in
    Svg.path
      [ stroke strokeStr
      , strokeWidth <| toString sw
      , fill fillStr
      , d dval ] []

toSvgElement : Style -> Shape -> Svg msg
toSvgElement style shape =
  case shape of
    Polygon { points } -> toPolygonElement style points
    Polyline { pts } -> toPolylineElement style pts
    Curve { point1, point2, point3, point4 } ->
      toCurveElement style point1 point2 point3 point4
    Path (v, beziers) ->
      toPathElement style v beziers
    x -> text "nothing"

toSvg : (Int, Int) -> Rendering -> Svg msg
toSvg bounds rendering =
  let
    (w, h) = bounds
    viewBoxValue = ["0", "0", toString w, toString h] |> String.join " "
    mirror = mirrorVector <| toFloat h
    toElement (shape, style) = toSvgElement style (mirrorShape mirror shape)
  in
    svg
      [ version "1.1", x "0", y "0", width (toString w), height (toString h) ]
      (rendering |> List.map toElement)
