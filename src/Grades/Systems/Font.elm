module Grades.Systems.Font exposing
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
import Grades.Levels.ABCPlus as Lvl
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (fontParser)
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
        showFrPlus show simplify next grade

    else
        showFrFull show simplify next grade


parse : String -> Maybe Grade
parse st =
    st
        |> Parser.run (fontParser Grade)
        |> Result.toMaybe


simplify : Grade -> Grade
simplify { n, cat } =
    Grade n cat Mod.Base


toLinearScale : Grade -> Float
toLinearScale _ =
    -1.0


fromLinearScale : Float -> Grade
fromLinearScale _ =
    zero


zero : Grade
zero =
    { n = 1, cat = Lvl.A, mod = Mod.Base }


next : Grade -> Grade
next { n, cat, mod } =
    if n <= 3 then
        Grade (n + 1) Lvl.A mod

    else if n <= 5 && Lvl.toABC cat == ABC.A then
        Grade n Lvl.B mod

    else if n <= 5 then
        Grade (n + 1) Lvl.A mod

    else
        case Lvl.next cat of
            Just lvl ->
                Grade n lvl mod

            Nothing ->
                Grade (n + 1) Lvl.A mod


prev : Grade -> Grade
prev { n, cat, mod } =
    if n <= 3 then
        Grade (n - 1) Lvl.A mod

    else if n <= 5 && Lvl.toABC cat /= ABC.A then
        Grade n Lvl.A mod

    else if n <= 5 then
        Grade (n - 1) Lvl.B mod

    else
        case Lvl.prev cat of
            Just lvl ->
                Grade n lvl mod

            Nothing ->
                Grade (n - 1) Lvl.CPlus mod


order : Grade -> Grade -> Order
order a b =
    let
        toTuple { n, cat, mod } =
            ( n, Lvl.toLinearScale cat, Mod.toLinearScale mod )
    in
    compare (toTuple a) (toTuple b)
