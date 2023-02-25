module Grades.Systems.Font exposing
    ( Grade
    , fromLinearScale
    , fromNum
    , next
    , order
    , parse
    , prev
    , show
    , simplify
    , toLinearScale
    , toNum
    , withMod
    , zero
    )

import Grades.Levels.ABC as ABC
import Grades.Levels.ABCPlus as Lvl
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (fontParser)
import Grades.Systems.Common exposing (..)
import Grades.Systems.Hueco as Hueco
import Grades.Util exposing (piecewise, splitNum)
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


withMod : Mod.Mod -> Grade -> Grade
withMod mod { n, cat } =
    Grade n cat mod


toLinearScale : Grade -> Float
toLinearScale =
    toNum >> piecewise 0.5 1.0 ( font4, v0 ) [] ( font7c, v9 )


fromLinearScale : Float -> Grade
fromLinearScale =
    piecewise 2.0 1.0 ( v0, font4 ) [] ( v9, font7c ) >> fromNum


v0 : Float
v0 =
    Hueco.toLinearScale (Hueco.Grade 0 Mod.Base)


v9 : Float
v9 =
    Hueco.toLinearScale (Hueco.Grade 9 Mod.Base)


font4 : Float
font4 =
    toNum (Grade 4 Lvl.A Mod.Base)


font7c : Float
font7c =
    toNum (Grade 7 Lvl.C Mod.Base)


toNum : Grade -> Float
toNum { n, cat, mod } =
    if n <= 3 then
        toFloat n + Mod.toLinearScale mod

    else if n <= 5 then
        4 + 2 * toFloat (n - 4) + (ABC.toLinearScale <| Lvl.toABC cat) * 3 + Mod.toLinearScale mod

    else
        8 + 6 * toFloat (n - 6) + Lvl.toLinearScale cat * 6 + Mod.toLinearScale mod


fromNum : Float -> Grade
fromNum x =
    -- Progression: 1  2  3  4  4+ 5  5+ 6a  6a+ 6b  6b+ 6c  6c+ 7a ...
    --              1  2  3  4  5  6  7  8   9   10  11  12  13  14 ...
    let
        ( n, delta ) =
            splitNum x
    in
    if n < 4 then
        let
            ( m, mod ) =
                Mod.fromLinearScale n delta
        in
        Grade m Lvl.A mod

    else if n < 8 then
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 4) // 2 + 4

            ( lvl, incr_ ) =
                case ( n + incr |> modBy 2, incr ) of
                    ( 0, 1 ) ->
                        ( Lvl.A, 1 )

                    ( 0, _ ) ->
                        ( Lvl.A, 0 )

                    ( 1, _ ) ->
                        ( Lvl.B, 0 )

                    _ ->
                        ( Lvl.C, 0 )
        in
        Grade (m + incr_) lvl mod

    else
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 8) // 6 + 6

            ( incr_, lvl ) =
                Lvl.fromIndex <| ((n - 8) |> modBy 6) + incr
        in
        Grade (m + incr_) lvl mod


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
