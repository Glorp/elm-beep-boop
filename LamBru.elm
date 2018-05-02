module LamBru exposing (bruToLamb, halfBruToLamb, lambToBru)

import Lamb.Exp as Lamb
import Bru.Exp as Bru

type Namer = Namer (String, List String) Int (String, List String)

namer : String -> List String -> Namer
namer x xs = Namer (x, xs) 0 (x, xs)

name : Namer -> String
name n =
  case n of
    Namer (s, rest) n x ->
      if n <= 0 then s else name (Namer (s, rest) (n - 1) x) ++ "'"

nextNamer : Namer -> Namer
nextNamer n =
  case n of
    Namer (s, []) n x -> Namer x (n + 1) x
    Namer (s, nexts :: rest) n x -> Namer (nexts, rest) n x

refName : List String -> Namer -> Int -> String
refName bound up n =
  let
    halpUp nextUp n =
      if n <= 0 then name nextUp else halpUp (nextNamer nextUp) (n - 1)
    halpBound nextBound n =
      case nextBound of
        [] -> halpUp up n
        s :: ss -> if n <= 0 then s else halpBound ss (n - 1)
  in
    halpBound bound n

bruLam : Namer -> Namer -> List String -> Bru.Exp -> Lamb.Exp
bruLam up down bound exp =
  case exp of
      Bru.Var n -> Lamb.Var (refName bound up n)
      Bru.App f a -> Lamb.App (bruLam up down bound f) (bruLam up down bound a)
      Bru.Lam b ->
        let
          p = name down
        in Lamb.Lam p (bruLam up (nextNamer down) (p :: bound) b)

upVars : Namer
upVars = namer "p" ["q", "u", "v", "w"]

downVars : Namer
downVars = namer "a" ["b", "c", "d", "e", "f", "x", "y", "z"]

bruToLamb : Bru.Exp -> Lamb.Exp
bruToLamb exp = bruLam upVars downVars [] exp

halfBruLam : Namer -> Namer -> List String -> Bru.HalfExp -> Lamb.Exp
halfBruLam up down bound exp =
  case exp of
      Bru.Empty -> Lamb.Var "___"
      Bru.HalfVar n -> Lamb.Var (refName bound up n ++ "?")
      Bru.HalfAppFun f -> Lamb.App (halfBruLam up down bound f) (Lamb.Var "...")
      Bru.HalfAppArg f a ->
        Lamb.App
          (bruLam up down bound f)
          (halfBruLam up down bound a)
      Bru.HalfLam b ->
        let
          p = name down
        in Lamb.Lam p (halfBruLam up (nextNamer down) (p :: bound) b)

halfBruToLamb : Bru.HalfExp -> Lamb.Exp
halfBruToLamb exp = halfBruLam upVars downVars [] exp

lamBru : Namer -> List String -> Lamb.Exp -> Bru.Exp
lamBru up bound exp =
  case exp of
    Lamb.Var s ->
      let
        lookupup u v n =
          if name u == v
          then n
          else lookupup (nextNamer u) v (n + 1)
        lookupbound b v n =
          case b of
            [] -> lookupup up v n
            x :: xs ->
              if x == v
              then n
              else lookupbound xs v (n + 1)
      in
        Bru.Var (lookupbound bound s 0)
    Lamb.Lam p b -> Bru.Lam (lamBru up (p :: bound) b)
    Lamb.App f a -> Bru.App (lamBru up bound f) (lamBru up bound a)

lambToBru : Lamb.Exp -> Bru.Exp
lambToBru exp = lamBru upVars [] exp
