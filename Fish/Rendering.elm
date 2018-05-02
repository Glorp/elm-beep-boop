module Fish.Rendering exposing (toSvg, toFittingSvg)

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


findVectorListBounds : List Vector -> ((Float, Float), (Float, Float))
findVectorListBounds vectors =
  let
    selectx {x, y} = x
    selecty {x, y} = y
    xs = vectors |> List.map selectx
    ys = vectors |> List.map selecty
    xmax = xs |> List.maximum |> Maybe.withDefault 0
    xmin = xs |> List.minimum |> Maybe.withDefault 0
    ymax = ys |> List.maximum |> Maybe.withDefault 0
    ymin = ys |> List.minimum |> Maybe.withDefault 0
  in
    ((xmin, ymin), (xmax, ymax))

findShapeBounds : Shape -> ((Float, Float), (Float, Float))
findShapeBounds shape =
  case shape of
    Polygon { points } -> findVectorListBounds points
    Polyline { pts } -> findVectorListBounds pts
    Curve { point1, point2, point3, point4 } ->
      findVectorListBounds [ point1, point2, point3, point4 ]
    Path (v, beziers) ->
      let
        gatherVectors { controlPoint1, controlPoint2, endPoint } = [ controlPoint1, controlPoint2, endPoint ]
        vs = beziers |> List.concatMap gatherVectors
      in
        findVectorListBounds (v :: vs)
    x -> ((0, 0), (0, 0))

findBounds : List Shape -> ((Float, Float), (Float, Float))
findBounds shapes =
  let
    boundses = shapes |> List.map findShapeBounds
    selectxmin ((xmin, ymin), (xmax, ymax)) = xmin
    selectymin ((xmin, ymin), (xmax, ymax)) = ymin
    selectxmax ((xmin, ymin), (xmax, ymax)) = xmax
    selectymax ((xmin, ymin), (xmax, ymax)) = ymax
    xmax = boundses |> List.map selectxmax |> List.maximum |> Maybe.withDefault 0
    xmin = boundses |> List.map selectxmin |> List.minimum |> Maybe.withDefault 0
    ymax = boundses |> List.map selectymax |> List.maximum |> Maybe.withDefault 0
    ymin = boundses |> List.map selectymin |> List.minimum |> Maybe.withDefault 0
  in
    ((xmin, ymin), (xmax, ymax))

getTransposer : ((Float, Float), (Float, Float)) -> Vector -> Vector
getTransposer ((xmin, ymin), (xmax, ymax)) {x, y} =
  { x = x - xmin
  , y = y - ymin }

getResizer : (Int, Int) -> ((Float, Float), (Float, Float)) -> Vector -> Vector
getResizer (w, h) ((xmin, ymin), (xmax, ymax)) { x, y } =
  let
    xdim = xmax - xmin
    ydim = ymax - ymin
    xf = (toFloat w) / xdim
    yf = (toFloat h) / ydim
    f = if xf < yf then xf else yf
  in
    { x = f * x
    , y = f * y }

toFittingSvg : (Int, Int) -> Rendering -> Svg msg
toFittingSvg bounds rendering =
  let
    (w, h) = bounds
    selectShape (shape, style) = shape
    outerBounds = rendering |> List.map selectShape |> findBounds
    transposer : Vector -> Vector
    transposer = getTransposer outerBounds
    resizer : Vector -> Vector
    resizer = getResizer bounds outerBounds
    viewBoxValue = ["0", "0", toString w, toString h] |> String.join " "
    mirror = mirrorVector <| toFloat h
    toElement (shape, style) =
      shape |> mirrorShape (transposer >> resizer >>  mirror) |> toSvgElement style
  in
    svg
      [ version "1.1", x "0", y "0", width (toString w), height (toString h) ]
      (rendering |> List.map toElement)
