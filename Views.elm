module Views exposing (viewFor)

import Html

import Mainly exposing (..)
import Bru.Exp as Bru
import Bru.String
import Lamb.Exp as Lamb
import Lamb.Eval as Eval
import Lamb.String
import LamBru
import Fishies

codeP : List String -> Html.Html msg
codeP strs =
  let
    h = List.intersperse (Html.br [] []) (List.map Html.text strs)
  in
    Html.p [] [Html.code [] h]

beopsH : State -> Html.Html msg
beopsH st = codeP [Bru.String.unparseBeopsMono (stateBeops st)]

bruH : State -> Html.Html msg
bruH st =
  case st of
    Half bru ->
      codeP [Bru.String.unparseHalf bru]
    Exec lam ->
      codeP [Bru.String.unparse (LamBru.lambToBru lam)]

lambH : State -> Html.Html msg
lambH st =
  case st of
    Half bru ->
      codeP [Lamb.String.unparse (LamBru.halfBruToLamb bru)]
    Exec lam ->
      codeP [Lamb.String.unparse lam]

execH : State -> Html.Html msg
execH st =
  case st of
    Exec lam -> codeP (Lamb.String.unparseExec (Eval.stepExec lam))
    _ -> lambH st

rectH : State -> Html.Html msg
rectH st =
  let
    l =
      case st of
        Half bru -> LamBru.halfBruToLamb bru
        Exec lam -> lam
  in
    Fishies.expToSvg (400, 400) l

fishH : State -> Html.Html msg
fishH st =
  let
    l =
      case st of
        Half bru -> LamBru.halfBruToLamb bru
        Exec lam -> lam
  in
    Fishies.expToFish (400, 400) l

onlyBeops : State -> Html.Html msg
onlyBeops st = Html.div [] [beopsH st]

bruNoEval : State -> Html.Html msg
bruNoEval st = Html.div [] [beopsH st, bruH st]

bruLambNoEval : State -> Html.Html msg
bruLambNoEval st = Html.div [] [beopsH st, bruH st, lambH st]

bruLamb : State -> Html.Html msg
bruLamb st = Html.div [] [beopsH st, bruH st, execH st]


rectangles : State -> Html.Html msg
rectangles st =
  Html.div [] [beopsH st, bruH st, execH st, rectH st]

onlyRectangles : State -> Html.Html msg
onlyRectangles st =
  case st of
    Exec _ -> Html.div [] [rectH st]
    _ -> rectangles st

fish : State -> Html.Html msg
fish st =
  Html.div [] [beopsH st, bruH st, execH st, fishH st]

onlyFish : State -> Html.Html msg
onlyFish st =
  case st of
    Exec _ -> Html.div [] [fishH st]
    _ -> fish st

viewFor : String -> State -> Html.Html msg
viewFor s =
  case s of
    "onlyBeops" -> onlyBeops
    "bruNoEval" -> bruNoEval
    "bruLambNoEval" -> bruLambNoEval
    "bruLamb" -> bruLamb
    "rectangles" -> rectangles
    "onlyRectangles" -> onlyRectangles
    "fish" -> fish
    "onlyFish" -> onlyFish
    _ -> rectangles
