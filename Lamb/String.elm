module Lamb.String exposing (unparse, unparseExec)

import Lamb.Exp exposing (..)
import Lamb.Eval exposing (..)

pstring : String -> String
pstring s = "(" ++ s ++ ")"

argstring : Exp -> String
argstring x =
  case x of
    Var s -> unparse (Var s)
    t -> pstring (unparse t)


unparse : Exp -> String
unparse x =
  case x of
    Var s -> s
    Lam p b -> "λ" ++ p ++ "." ++ unparse b
    App (Lam p b) a -> pstring (unparse (Lam p b)) ++ " " ++ argstring a
    App f a -> unparse f ++ " " ++ argstring a

unparseExec : Exec -> List String
unparseExec ex =
  case ex of
    Normal x -> [unparse x]
    Reduce oldx newx -> [unparse oldx, unparse newx]
    Rename oldv oldx newv newx ->
      [unparse oldx ++ " | [" ++ newv ++ "/" ++ oldv ++ "]", unparse newx]
