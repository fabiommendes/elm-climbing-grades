module Grades.Systems.Fr exposing
    ( Grade
    , fromLinearScale
    , next
    , order
    , parse
    , prev
    , show
    , simplify
    , toLinearScale
    , zero
    )

import Grades.Levels.ABC as ABC
import Grades.Levels.ABCPlus as Lvl exposing (..)
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (frParser)
import Grades.Systems.Common exposing (..)
import Parser


type alias Grade =
    { n : Int
    , cat : Lvl.Level
    , mod : Mod.Mod
    }


show : Grade -> String
show grade =
    if grade.n <= 3 then
        showFrNumber show simplify next grade

    else if grade.n <= 5 then
        showFrCat show simplify next grade

    else
        showFrFull show simplify next grade


parse : String -> Maybe Grade
parse st =
    st
        |> Parser.run (frParser Grade)
        |> Result.toMaybe


simplify : Grade -> Grade
simplify g =
    g


fromLinearScale : Float -> Grade
fromLinearScale _ =
    zero


toLinearScale : Grade -> Float
toLinearScale _ =
    -1.0


zero : Grade
zero =
    Grade 1 A Mod.Base


next : Grade -> Grade
next { n, cat, mod } =
    if n <= 3 then
        Grade (n + 1) A mod

    else if n <= 5 then
        case Lvl.toABC cat of
            ABC.A ->
                Grade n B mod

            ABC.B ->
                Grade n C mod

            ABC.C ->
                Grade (n + 1) A mod

    else
        case Lvl.next cat of
            Just lvl ->
                Grade n lvl mod

            Nothing ->
                Grade (n + 1) A mod


prev : Grade -> Grade
prev { n, cat, mod } =
    if n <= 3 then
        Grade (n - 1) A mod

    else if n <= 5 then
        case Lvl.toABC cat of
            ABC.A ->
                Grade (n - 1) (select (n == 4) A C) mod

            ABC.B ->
                Grade n A mod

            ABC.C ->
                Grade n B mod

    else
        case Lvl.prev cat of
            Just lvl ->
                Grade n lvl mod

            Nothing ->
                Grade (n - 1) CPlus mod


order : Grade -> Grade -> Order
order a b =
    let
        toTuple { n, cat, mod } =
            ( n, Lvl.toLinearScale cat, Mod.toLinearScale mod )
    in
    compare (toTuple a) (toTuple b)
