module Fish.Picture exposing (..)

import Fish.Box exposing (..)
import Fish.Shape exposing (..)
import Fish.Style exposing (..)

type alias Rendering = List (Shape, Style)

type alias Picture = Box -> Rendering

blank : Picture
blank _ = []

times : Int -> (a -> a) -> (a -> a)
times n fn =
  if n == 0 then identity
  else fn >> times (n - 1) fn

turn : Picture -> Picture
turn p = turnBox >> p

cturn : Picture -> Picture
cturn = turn >> turn >> turn

flip : Picture -> Picture
flip p = flipBox >> p

toss : Picture -> Picture
toss p = tossBox >> p

aboveRatio : Int -> Int -> Picture -> Picture -> Picture
aboveRatio m n p1 p2 =
  \box ->
    let
      f = toFloat m / toFloat (m + n)
      top = box |> moveVertically (1 - f) |> scaleVertically f
      bot = box |> scaleVertically (1 - f)
    in
      (p1 top) ++ (p2 bot)

above : Picture -> Picture -> Picture
above = aboveRatio 1 1

besideRatio : Int -> Int -> Picture -> Picture -> Picture
besideRatio m n p1 p2 =
  \box ->
    let
      f = toFloat m / toFloat (m + n)
      left = box |> scaleHorizontally f
      right = box |> moveHorizontally f |> scaleHorizontally (1 - f)
    in
      (p1 left) ++ (p2 right)

beside : Picture -> Picture -> Picture
beside = besideRatio 1 1

quartet : Picture -> Picture -> Picture -> Picture -> Picture
quartet nw ne sw se =
  above (beside nw ne)
        (beside sw se)

nonet : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
nonet nw nm ne mw mm me sw sm se =
  let
    row : Picture -> Picture -> Picture -> Picture
    row w m e =
      besideRatio 1 2 w (beside m e)
    col : Picture -> Picture -> Picture -> Picture
    col n m s =
      aboveRatio 1 2 n (above m s)
  in
    col (row nw nm ne)
        (row mw mm me)
        (row sw sm se)

over : Picture -> Picture -> Picture
over p1 p2 =
  \box -> (p1 box) ++ (p2 box)

lamtile : Picture -> Picture -> Picture
lamtile p1 p2 =
  let
    pe = p2 |> toss |> flip |> turn |> turn |> turn
  in
    pe |> over p1

apptile : Picture -> Picture -> Picture
apptile p1 p2 =
  let
    pn = p1 |> toss |> flip
    ps = p2 |> toss |> flip |> turn |> turn
  in
    ps |> over pn

restrokeStyle : String -> StrokeStyle -> StrokeStyle
restrokeStyle n { strokeWidth, strokeColor } =
  { strokeWidth = strokeWidth
  , strokeColor = if strokeColor == "White" then strokeColor else n }

refillStyle : String -> FillStyle -> FillStyle
refillStyle n { fillColor } =
  { fillColor = if fillColor == "White" then fillColor else n }

restyle : String -> Style -> Style
restyle n style =
  let
    { stroke, fill } = style
  in
    { stroke = stroke |> Maybe.map (restrokeStyle n)
    , fill = fill |> Maybe.map (refillStyle n) }

recolorItem : String -> (Shape, Style) -> (Shape, Style)
recolorItem n (shape, style) =
  (shape, restyle n style)

recolor : String -> Picture -> Picture
recolor n p =
  \box ->
    p box |> List.map (recolorItem n)

toColorCode : String -> String
toColorCode n =
  if n == "x" || n == "x?" then "#4FA4A5"
  else if n == "y" || n == "y?" then "#D47564"
  else if n == "z" || n == "z?" then "#41F4C4"
  else if n == "f" || n == "f?" then "#5C6182"
  else if n == "a" || n == "a?" then "#F4B41B"
  else if n == "b" || n == "b?" then "#F47E1B"
  else if n == "c" || n == "c?" then "#E6482E"
  else if n == "d" || n == "d?" then "#A93B3B"
  else if n == "e" || n == "e?" then "#429EF4"
  else "Black"

vartile : String -> Picture -> Picture
vartile n p = recolor (toColorCode n) p

boxvartile : String -> Picture -> Picture
boxvartile = vartile

boxlamtile : Picture -> Picture -> Picture
boxlamtile = aboveRatio 1 4

boxapptile : Picture -> Picture -> Picture
boxapptile = beside

ttile : Picture -> Picture
ttile fish =
  let
    fishN = fish |> toss |> flip
    fishE = fishN |> turn |> turn |> turn
  in
    -- over fish (over fishN fishE)
    fishE |> over fishN |> over fish

utile : Picture -> Picture
utile fish =
  let
    fishN = fish |> toss |> flip
    fishW = fishN |> turn
    fishS = fishW |> turn
    fishE = fishS |> turn
  in
    fishE |> over fishS |> over fishW |> over fishN

side : Int -> Picture -> Picture -> Picture
side n p fish =
  if n == 0 then p
  else
    let
      s = side (n - 1) p fish
      t = ttile fish
    in
      quartet s s (turn t) t

corner : Int -> Picture -> Picture -> Picture
corner n p fish =
  if n == 0 then p
  else
    let
      c = corner (n - 1) p fish
      s = side (n - 1) p fish
    in
      quartet c s (turn s) (utile fish)

squareLimit : Int -> Picture -> Picture -> Picture -> Picture
squareLimit n p center fish =
  let
    c = corner n p fish
    s = side n p fish
    nw = c
    nm = s
    ne = c |> turn |> turn |> turn
    mw = s |> turn
    mm = center
    me = s |> turn |> turn |> turn
    sw = c |> turn
    sm = s |> turn |> turn
    se = c |> turn |> turn
  in
    nonet nw nm ne mw mm me sw sm se
