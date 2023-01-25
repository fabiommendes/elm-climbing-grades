module Grades.Systems.Us exposing (Grade, fromLinearScale, next, order, parse, prev, show, simplify, toLinearScale, zero)

import Grades.Levels.ABCD as Lvl
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (usParser)
import Parser


type alias Grade =
    { n : Int
    , cat : Lvl.Level
    , mod : Mod.Mod
    }


show : Grade -> String
show { n, cat } =
    if n < 0 then
        String.fromInt (max n -5 + 5)

    else if n < 10 then
        "5." ++ String.fromInt n

    else
        "5." ++ String.fromInt n ++ Lvl.show cat


parse : String -> Maybe Grade
parse st =
    st
        |> Parser.run (usParser Grade)
        |> Result.toMaybe


simplify : Grade -> Grade
simplify { n, cat } =
    if n < 10 then
        { n = n, cat = Lvl.A, mod = Mod.Base }

    else
        { n = n, cat = cat, mod = Mod.Base }


fromLinearScale : Float -> Grade
fromLinearScale _ =
    zero


toLinearScale : Grade -> Float
toLinearScale _ =
    0.0


zero : Grade
zero =
    Grade -4 Lvl.A Mod.Base


next : Grade -> Grade
next { n, cat, mod } =
    if n < 10 then
        Grade (n + 1) Lvl.A mod

    else
        case Lvl.next cat of
            Just lvl ->
                Grade n lvl mod

            _ ->
                Grade (n + 1) Lvl.A mod


prev : Grade -> Grade
prev { n, cat, mod } =
    if n < 10 then
        Grade (n - 1) Lvl.A mod

    else
        case Lvl.prev cat of
            Just lvl ->
                Grade n lvl mod

            _ ->
                Grade (n - 1) Lvl.D mod


order : Grade -> Grade -> Order
order a b =
    let
        toTuple { n, cat, mod } =
            ( n, Lvl.toLinearScale cat, Mod.toLinearScale mod )
    in
    compare (toTuple a) (toTuple b)
