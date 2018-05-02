module Fishies exposing (expToSvg)

import Fish.Box exposing (..)
import Fish.Boxy exposing (boxThing)
import Fish.Fitting exposing (createPicture, createStylePicture)
import Fish.Picture exposing (..)
import Fish.Rendering exposing (toSvg)
import Lamb.Exp exposing(..)
import Svg exposing (Svg)

convertExpBox : Picture -> Exp -> Picture
convertExpBox p exp =
  case exp of
    Lam n body -> boxlamtile (boxvartile n p) (convertExpBox p body)
    App e1 e2 -> boxapptile (convertExpBox p e1) (convertExpBox p e2)
    Var v -> boxvartile v p

expToSvg : (Int, Int) -> Exp -> Svg msg
expToSvg wh exp =
  let
    (w, h) = wh
    box =
      { a = { x = 0, y = 0 }
      , b = { x = toFloat w, y = 0 }
      , c = { x = 0, y = toFloat h } }
    boxpicture = createStylePicture boxThing
  in
    box
      |> convertExpBox boxpicture exp
      |> toSvg wh
