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

onlyBeops : State -> Html.Html msg
onlyBeops st = Html.div [] [beopsH st]

bruNoEval : State -> Html.Html msg
bruNoEval st = Html.div [] [beopsH st, bruH st]

bruLambNoEval : State -> Html.Html msg
bruLambNoEval st = Html.div [] [beopsH st, bruH st, lambH st]

execH : Lamb.Exp -> Html.Html msg
execH x = codeP (Lamb.String.unparseExec (Eval.stepExec x))

bruLamb : State -> Html.Html msg
bruLamb st =
  case st of
    Half _ -> bruLambNoEval st
    Exec lam ->
      Html.div [] [beopsH st, bruH st, execH lam]

rectangles : State -> Html.Html msg
rectangles st =
  case st of
    Half _ -> bruLambNoEval st
    Exec lam ->
      Html.div []
        [beopsH st
        , bruH st
        , execH lam
        , Fishies.expToSvg (400, 400) lam
        ]


viewFor : String -> State -> Html.Html msg
viewFor s =
  case s of
    "onlyBeops" -> onlyBeops
    "bruNoEval" -> bruNoEval
    "bruLambNoEval" -> bruLambNoEval
    "bruLamb" -> bruLamb
    "rectangles" -> rectangles
    _ -> rectangles
