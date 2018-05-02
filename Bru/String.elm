module Bru.String exposing (unparse, unparseHalf, unparseMaybe, unparseBeopMono, unparseBeop, unparseBeopsMono)

import Bru.Exp exposing (..)

pstring : String -> String
pstring s = "(" ++ s ++ ")"

argstring : Exp -> String
argstring x =
  case x of
    Var n -> unparse (Var n)
    t -> pstring (unparse t)

unparse : Exp -> String
unparse x =
  case x of
    Var n -> toString (n + 1)
    Lam b -> "λ" ++ unparse b
    App (Lam b) a -> pstring (unparse (Lam b)) ++ " " ++ argstring a
    App f a -> unparse f ++ " " ++ argstring a

argstringHalf : HalfExp -> String
argstringHalf x =
  case x of
    HalfVar n -> unparseHalf (HalfVar n)
    Empty -> unparseHalf Empty
    t -> pstring (unparseHalf t)

unparseHalf : HalfExp -> String
unparseHalf x =
  case x of
    Empty -> "___"
    HalfVar n -> toString (n + 1) ++ "?"
    HalfLam b -> "λ" ++ unparseHalf b
    HalfAppFun (HalfLam b) -> pstring (unparseHalf (HalfLam b)) ++ " " ++ "..."
    HalfAppFun f -> unparseHalf f ++ " " ++ "..."
    HalfAppArg (Lam b) a -> pstring (unparse (Lam b)) ++ " " ++ argstringHalf a
    HalfAppArg f a -> unparse f ++ " " ++ argstringHalf a

unparseMaybe : MaybeExp -> String
unparseMaybe x =
  case x of
    Full t -> "ok:    " ++ unparse t
    Half t -> "...    " ++ unparseHalf t
    Ohno t -> "oh no: " ++ unparseHalf t

unparseBeopMono : Beop -> String
unparseBeopMono b =
  case b of
    Beep -> "beep  "
    Boop -> "boop  "
    Bap -> "bap   "
    Pling -> "pling "

unparseBeop : Beop -> String
unparseBeop b =
  case b of
    Beep -> "beep"
    Boop -> "boop"
    Bap -> "bap"
    Pling -> "pling"

unparseBeopsMono : List Beop -> String
unparseBeopsMono bps =
  let
    halp a b = unparseBeopMono a ++ " " ++ b
  in
    List.foldl halp "" bps
