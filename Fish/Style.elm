module Fish.Style exposing (..)

type alias StrokeStyle = 
  { strokeWidth : Float
  , strokeColor : String }

type alias FillStyle =
  { fillColor : String }

type alias Style =
  { stroke : Maybe StrokeStyle
  , fill : Maybe FillStyle }
