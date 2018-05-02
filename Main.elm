port module Main exposing (main)

import Html

import Mainly exposing (..)
import Bru.Exp as Bru
import Bru.String
import Lamb.Exp as Lamb
import Lamb.Eval as Eval
import LamBru
import Views

port beopIn : (String -> msg) -> Sub msg
port beopOut : List String -> Cmd msg

type alias Flags = { view : String }

type alias Model msg = (State, State -> Html.Html msg)

bpOut : List Bru.Beop -> Cmd msg
bpOut bps = beopOut (List.map (Bru.String.unparseBeop) (List.reverse bps))

addBeop : State -> Bru.Beop -> (State, Cmd msg)
addBeop st bp =
  case st of
    Half x -> beopsState (bp :: Bru.halfBruToBeops x)
    x -> (x, Cmd.none)

removeBeop : State -> (State, Cmd msg)
removeBeop st =
  let
    bps =
      case stateBeops st of
        [] -> []
        x :: xs -> xs
  in
    beopsState bps

beopsState : List Bru.Beop -> (State, Cmd msg)
beopsState bps =
  case Bru.beopsToBru bps of
    Bru.Full b -> (Exec (LamBru.bruToLamb b), bpOut bps)
    Bru.Half b -> (Half b, Cmd.none)
    Bru.Ohno b -> (Half b, Cmd.none)

nextState : State -> (State, Cmd msg)
nextState st =
  case st of
    Exec exp ->
      case Eval.stepExec exp of
        Eval.Normal x -> (Exec x, Cmd.none)
        Eval.Rename _ _ _ x -> (Exec x, bpOut (lambToBeops x))
        Eval.Reduce _ x -> (Exec x, bpOut (lambToBeops x))
    x -> (x, Cmd.none)

updateState : String -> State -> (State, Cmd msg)
updateState s st =
  case s of
    "beep" -> addBeop st Bru.Beep
    "boop" -> addBeop st Bru.Boop
    "bap" -> addBeop st Bru.Bap
    "pling" -> addBeop st Bru.Pling
    "undo" -> removeBeop st
    "next" -> nextState st
    _ -> (st, Cmd.none)

update : String -> Model msg -> (Model msg, Cmd msg)
update s m =
  case m of
    (st, v) ->
      case updateState s st of
        (newst, c) -> ((newst, v), c)

init : Flags -> (Model msg, Cmd msg )
init flags = ((Half Bru.Empty, Views.viewFor flags.view ), Cmd.none)

view : Model msg -> Html.Html msg
view m =
  case m of
    (st, v) -> v st

main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = (\m -> beopIn identity)
        , view = view
        }
