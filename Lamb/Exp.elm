module Lamb.Exp exposing (Exp(..), Step(..), Path, Found(..), allIds, freeIds, findExp, assemblePath)

import Set

type Exp
  = Var String
  | Lam String Exp
  | App Exp Exp

type Step
  = LamStep String
  | AppStepF Exp
  | AppStepA Exp

type alias Path = List Step

type Found a = Found Path a

findExp : (Exp -> Maybe a) -> Exp -> Maybe (Found a)
findExp pred expr =
  let
    find pt x =
      case pred x of
        Just res -> Just (Found pt res)
        Nothing ->
          case x of
            Var _ -> Nothing
            Lam p b -> find (LamStep p :: pt) b
            App f a ->
              case find (AppStepF a :: pt) f of
                Just res -> Just res
                Nothing -> find (AppStepA f :: pt) a
  in
    find [] expr

allIds : Exp -> Set.Set String
allIds x =
  case x of
    Var s -> Set.singleton s
    Lam p b -> Set.insert p (allIds b)
    App f a -> Set.union (allIds f) (allIds a)

freeIds : Exp -> Set.Set String
freeIds exp =
  let
    halp bound x =
      case x of
        Var s -> if Set.member s bound then Set.empty else Set.singleton s
        Lam p b -> halp (Set.insert p bound) b
        App f a -> Set.union (halp bound f) (halp bound a)
  in
    halp Set.empty exp

assemble : Step -> Exp -> Exp
assemble s x =
  case s of
    LamStep p -> Lam p x
    AppStepF a -> App x a
    AppStepA f -> App f x

assemblePath : Path -> Exp -> Exp
assemblePath p x = List.foldl assemble x p
