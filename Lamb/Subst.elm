module Lamb.Subst exposing (Redex(..), redex, subst)

import Lamb.Exp exposing (..)

type Redex = Redex Exp String Exp

redex : Exp -> Maybe Redex
redex x =
  case x of
    App (Lam p b) a -> Just (Redex a p b)
    _ -> Nothing


subst : Redex -> Exp
subst r =
  case r of
    Redex arg param body ->
      let
        halp x =
          case x of
            App f a -> App (halp f) (halp a)
            Lam p b -> Lam p (if p == param then b else halp b)
            Var s -> if s == param then arg else (Var s)
      in
        halp body
