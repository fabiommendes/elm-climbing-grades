module Grades.Systems.Fr exposing
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
import Grades.Levels.ABCPlus as Lvl exposing (..)
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (frParser)
import Grades.Systems.Common exposing (..)
import Grades.Systems.Us as Us
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
        showFrCat show simplify next grade

    else
        showFrFull show simplify next grade


parse : String -> Maybe Grade
parse st =
    st
        |> Parser.run (frParser Grade)
        |> Result.toMaybe


simplify : Grade -> Grade
simplify { n, cat } =
    Grade n cat Mod.Base


withMod : Mod.Mod -> Grade -> Grade
withMod mod { n, cat } =
    Grade n cat mod


toLinearScale : Grade -> Float
toLinearScale =
    toNum >> piecewise 0.75 1.0 ( fr1, us3 ) [] ( fr7a, us511d )


fromLinearScale : Float -> Grade
fromLinearScale =
    piecewise (1 / 0.75) 1.0 ( us3, fr1 ) [] ( us511d, fr7a ) >> fromNum


us511d : Float
us511d =
    Us.toLinearScale (Us.parse "5.11d" |> Maybe.withDefault Us.zero)


us3 : Float
us3 =
    Us.toLinearScale (Us.parse "3" |> Maybe.withDefault Us.zero)


fr7a : Float
fr7a =
    toNum (Grade 7 Lvl.A Mod.Base)


fr1 : Float
fr1 =
    toNum (Grade 1 Lvl.A Mod.Base)


toNum : Grade -> Float
toNum { n, cat, mod } =
    -- Progression: 1  2  3 4a 4b 4c 5a 5b 5c 6a+  6b  6b+ 6c  6c+ 7a ...
    --              1  2  3  4  5  6  7  8  9  10  11  12  13  14  15 ...
    if n <= 3 then
        toFloat n + Mod.toLinearScale mod

    else if n <= 5 then
        4 + 3 * toFloat (n - 4) + (ABC.toLinearScale <| Lvl.toABC cat) * 3 + Mod.toLinearScale mod

    else
        10 + 6 * toFloat (n - 6) + Lvl.toLinearScale cat * 6 + Mod.toLinearScale mod


fromNum : Float -> Grade
fromNum x =
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

    else if n < 10 then
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 4) // 3 + 4

            ( incr_, lvl ) =
                ABC.fromIndex <| ((n - 4) |> modBy 3) + incr
        in
        Grade (m + incr_) (Lvl.fromABC lvl) mod

    else
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 10) // 6 + 6

            ( incr_, lvl ) =
                Lvl.fromIndex <| ((n - 10) |> modBy 6) + incr
        in
        Grade (m + incr_) lvl mod


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
