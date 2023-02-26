module Grades.Systems.Fr exposing
    ( Grade
    , fromLinearScale
    , parse
    , show
    , toLinearScale
    , zero
    )

import Grades.Levels.ABC as ABC
import Grades.Levels.ABCPlus as Lvl exposing (..)
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (frParser)
import Grades.Systems.Common exposing (..)
import Grades.Systems.Us as Us
import Grades.Util exposing (normalizeNum, piecewise, splitNum)
import Parser


type alias Grade =
    Float


grade : Int -> Level -> Mod.Mod -> Float
grade n cat mod =
    -- Progression: 1  2  3 4a 4b 4c 5a 5b 5c 6a+  6b  6b+ 6c  6c+ 7a ...
    --              1  2  3  4  5  6  7  8  9  10  11  12  13  14  15 ...
    if n <= 3 then
        toFloat n + Mod.toLinearScale mod

    else if n <= 5 then
        4 + 3 * toFloat (n - 4) + (ABC.toLinearScale <| Lvl.toABC cat) * 3 + Mod.toLinearScale mod

    else
        10 + 6 * toFloat (n - 6) + Lvl.toLinearScale cat * 6 + Mod.toLinearScale mod


split : Grade -> { n : Int, cat : Lvl.Level, mod : Mod.Mod }
split x =
    let
        ( n, delta ) =
            splitNum x
    in
    if n < 4 then
        let
            ( m, mod ) =
                Mod.fromLinearScale n delta
        in
        { n = m, cat = Lvl.A, mod = mod }

    else if n < 10 then
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 4) // 3 + 4

            ( incr_, lvl ) =
                ABC.fromIndex <| ((n - 4) |> modBy 3) + incr
        in
        { n = m + incr_, cat = Lvl.fromABC lvl, mod = mod }

    else
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 10) // 6 + 6

            ( incr_, lvl ) =
                Lvl.fromIndex <| ((n - 10) |> modBy 6) + incr
        in
        { n = m + incr_, cat = lvl, mod = mod }


show : Grade -> String
show x =
    if x <= Mod.halfwayNext 3 then
        showFrNumber show split x

    else if x <= Mod.halfwayNext 7 then
        showFrCat show split x

    else
        showFrFull show split x


parse : String -> Maybe Grade
parse st =
    st
        |> Parser.run (frParser grade)
        |> Result.toMaybe


toLinearScale : Grade -> Float
toLinearScale =
    piecewise 0.75 1.0 ( fr1, us3 ) [] ( fr7a, us511d )


fromLinearScale : Float -> Grade
fromLinearScale =
    piecewise (1 / 0.75) 1.0 ( us3, fr1 ) [] ( us511d, fr7a ) >> normalizeNum


us511d : Float
us511d =
    Us.parse "5.11d" |> Maybe.withDefault Us.zero


us3 : Float
us3 =
    Us.parse "3" |> Maybe.withDefault Us.zero


fr7a : Float
fr7a =
    grade 7 Lvl.A Mod.Base


fr1 : Float
fr1 =
    grade 1 Lvl.A Mod.Base


zero : Grade
zero =
    grade 1 A Mod.Base
