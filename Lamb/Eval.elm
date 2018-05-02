module Lamb.Eval exposing (Exec(..), stepExec)

import Lamb.Exp exposing (..)
import Lamb.Subst exposing (..)
import Set

type Exec
  = Reduce Exp Exp
  | Rename String Exp String Exp
  | Normal Exp

type Conflict = Conflict String Exp

conflict : String -> Set.Set String -> Exp -> Maybe Conflict
conflict param bad x =
  case x of
    Lam p b ->
      if p /= param && Set.member p bad && Set.member param (freeIds b)
      then Just (Conflict p b)
      else Nothing
    _ -> Nothing

uniqueId : Set.Set String -> String -> String
uniqueId used str =
  if Set.member str used then uniqueId used (str ++ "*") else str

type Renamed = Ren String String Exp
rename : Exp -> Found Conflict -> Renamed
rename exp fc =
  case fc of
    Found pt (Conflict p b) ->
      let
        newId = uniqueId (allIds exp) p
        nonflict = assemblePath pt (Lam newId (subst (Redex (Var newId) p b)))
      in
        Ren p newId nonflict

findConflict : Set.Set String -> Redex -> Maybe (Found Conflict)
findConflict argFree r =
  case r of
    Redex arg param body ->
      let
        find pt exp =
          case conflict param argFree exp of
            Just c -> Just (Found pt c)
            Nothing ->
              case exp of
                Var _ -> Nothing
                Lam p b ->
                  if p == param
                  then Nothing
                  else find (LamStep p :: pt) b
                App f a ->
                  case find (AppStepF a :: pt) f of
                    Just x -> Just x
                    Nothing -> find (AppStepA f :: pt) a
      in find [] body

stepExec : Exp -> Exec
stepExec exp =
  case findExp redex exp of
    Nothing -> Normal exp
    Just (Found tp r) ->
      let
        renameHalp f r =
          case (f, r) of
            (Found tp (Redex arg param body), Ren old new t) ->
              Rename old exp new (assemblePath tp ((App (Lam param t)) arg))

        reduceRename f mc =
          case (f, mc) of
            (Found tp r, Nothing) -> Reduce exp (assemblePath tp (subst r))
            (f, Just c) -> renameHalp f (rename exp c)

        findConf (Redex a p b) = findConflict (freeIds a) (Redex a p b)
    in
      reduceRename (Found tp r) (findConf r)
