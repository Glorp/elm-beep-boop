module Bru.Exp exposing (Exp(..), Beop(..), HalfExp(..), MaybeExp(..), addBeop, addBeopMaybe, beopsToBru, bruToBeops, halfBruToBeops)

type Exp
  = Var Int
  | Lam Exp
  | App Exp Exp

type Beop
  = Beep
  | Boop
  | Bap
  | Pling

type HalfExp
  = Empty
  | HalfVar Int
  | HalfLam HalfExp
  | HalfAppFun HalfExp
  | HalfAppArg Exp HalfExp

type MaybeExp
  = Full Exp
  | Half HalfExp
  | Ohno HalfExp

start : Beop -> MaybeExp
start bp =
  case bp of
    Beep -> Half (HalfLam Empty)
    Boop -> Half (HalfVar 0)
    Pling -> Half (HalfAppFun Empty)
    _ -> Ohno Empty

varAdd : Int -> Beop -> MaybeExp
varAdd n bp =
  case bp of
    Boop -> Half (HalfVar (n + 1))
    Bap -> Full (Var n)
    _ -> Ohno (HalfVar n)

lammy : MaybeExp -> MaybeExp
lammy t =
  case t of
    Full b -> Full (Lam b)
    Half b -> Half (HalfLam b)
    Ohno b -> Ohno (HalfLam b)

appyF : MaybeExp -> MaybeExp
appyF t =
  case t of
    Full f -> Half (HalfAppArg f Empty)
    Half f -> Half (HalfAppFun f)
    Ohno f -> Ohno (HalfAppFun f)

appyA : Exp -> MaybeExp -> MaybeExp
appyA f t =
  case t of
    Full a -> Full (App f a)
    Half a -> Half (HalfAppArg f a)
    Ohno a -> Ohno (HalfAppArg f a)

addBeop : HalfExp -> Beop -> MaybeExp
addBeop t bp =
  case t of
    Empty -> start bp
    HalfVar n -> varAdd n bp
    HalfLam b -> lammy (addBeop b bp)
    HalfAppFun f -> appyF (addBeop f bp)
    HalfAppArg f a -> appyA f (addBeop a bp)

addBeopMaybe : MaybeExp -> Beop -> MaybeExp
addBeopMaybe x bp =
  case x of
    Half t -> addBeop t bp
    t -> t

beopsToBru : List Beop -> MaybeExp
beopsToBru beops =
  List.foldr (flip addBeopMaybe) (Half Empty) beops

bruBeops : List Beop -> Exp -> List Beop
bruBeops res exp =
  case exp of
    Var n ->
      let
        halp r i =
          if i <= 0
          then Bap :: Boop :: r
          else halp (Boop :: r) (i - 1)
      in
        halp res n
    Lam x -> bruBeops (Beep :: res) x
    App f a -> bruBeops (bruBeops (Pling :: res) f) a

bruToBeops : Exp -> List Beop
bruToBeops exp = bruBeops [] exp

halfBruBeops : List Beop -> HalfExp -> List Beop
halfBruBeops res exp =
  case exp of
    Empty -> res
    HalfVar n ->
      let
        halp r i =
          if i <= 0
          then Boop :: r
          else halp (Boop :: r) (i - 1)
      in
        halp res n
    HalfLam x -> halfBruBeops (Beep :: res) x
    HalfAppFun f -> halfBruBeops (Pling :: res) f
    HalfAppArg f a -> halfBruBeops (bruBeops (Pling :: res) f) a

halfBruToBeops : HalfExp -> List Beop
halfBruToBeops exp = halfBruBeops [] exp
