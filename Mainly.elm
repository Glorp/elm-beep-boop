module Mainly exposing (State(..), stateBeops, lambToBeops)

import Html
import Lamb.Exp as Lamb
import Bru.Exp as Bru
import LamBru

type State
  = Half Bru.HalfExp
  | Exec Lamb.Exp


stateBeops : State -> List Bru.Beop
stateBeops st =
  case st of
    Half x -> Bru.halfBruToBeops x
    Exec x -> lambToBeops x

lambToBeops : Lamb.Exp -> List Bru.Beop
lambToBeops exp = Bru.bruToBeops (LamBru.lambToBru exp)
