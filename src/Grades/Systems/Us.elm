module Grades.Systems.Us exposing (Grade, fromLinearScale, next, order, parse, prev, show, simplify, toLinearScale, withMod, zero)

import Grades.Levels.ABCD as Lvl
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (usParser)
import Grades.Util exposing (splitNum)
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


withMod : Mod.Mod -> Grade -> Grade
withMod mod { n, cat } =
    Grade n cat mod


fromLinearScale : Float -> Grade
fromLinearScale x =
    let
        ( n, delta ) =
            splitNum x
    in
    if n < 10 then
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta
        in
        Grade (n + incr) Lvl.A mod

    else
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 10) // 4 + 10

            ( incr_, lvl ) =
                Lvl.fromIndex <| ((n - 10) |> modBy 4) + incr
        in
        Grade (m + incr_) lvl mod


toLinearScale : Grade -> Float
toLinearScale { n, mod, cat } =
    -- Grades  1  2  3  4 5.0  5.1  5.2  5.3 ... 5.9  5.10a 5.10b ...
    -- Nums   -4 -3 -2 -1   0    1    2    3       9     10    11 ...
    if n < 10 then
        toFloat n + Mod.toLinearScale mod

    else
        4 * (toFloat n - 10) + 10 + Mod.toLinearScale mod + 4 * Lvl.toLinearScale cat


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
