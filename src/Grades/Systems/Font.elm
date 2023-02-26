module Grades.Systems.Font exposing
    ( Grade
    , fromLinearScale
    , grade
    , parse
    , show
    , split
    , toLinearScale
    , zero
    )

import Grades.Levels.ABC as ABC
import Grades.Levels.ABCPlus as Lvl
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (fontParser)
import Grades.Systems.Common exposing (..)
import Grades.Systems.Hueco as Hueco
import Grades.Util exposing (normalizeNum, piecewise, splitNum)
import Parser


{-| Numeric representation of grade progression

    Progression: 1  2  3  4  4+ 5  5+ 6a  6a+  6b  6b+  6c  6c+  7a ...
                 1  2  3  4  5  6  7   8   9   10  11   12  13   14 ...

-}
type alias Grade =
    Float


grade : Int -> Lvl.Level -> Mod.Mod -> Grade
grade n cat mod =
    if n <= 3 then
        toFloat n + Mod.toLinearScale mod

    else if n <= 5 then
        4 + 2 * toFloat (n - 4) + (ABC.toLinearScale <| Lvl.toABC cat) * 3 + Mod.toLinearScale mod

    else
        8 + 6 * toFloat (n - 6) + Lvl.toLinearScale cat * 6 + Mod.toLinearScale mod


split : Float -> { n : Int, cat : Lvl.Level, mod : Mod.Mod }
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
        { n = m + incr_, cat = lvl, mod = mod }

    else
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 8) // 6 + 6

            ( incr_, lvl ) =
                Lvl.fromIndex <| ((n - 8) |> modBy 6) + incr
        in
        { n = m + incr_, cat = lvl, mod = mod }


show : Grade -> String
show x =
    if x <= Mod.halfwayNext 3 then
        showFrNumber show split x

    else if x <= Mod.halfwayNext 7 then
        showFrPlus show split x

    else
        showFrFull show split x


parse : String -> Maybe Grade
parse st =
    st
        |> Parser.run (fontParser grade)
        |> Result.toMaybe


toLinearScale : Grade -> Float
toLinearScale =
    piecewise 0.5 1.0 ( font4, v0 ) [] ( font7c, v9 )


fromLinearScale : Float -> Grade
fromLinearScale =
    piecewise 2.0 1.0 ( v0, font4 ) [] ( v9, font7c ) >> normalizeNum


v0 : Float
v0 =
    Hueco.grade 0 Mod.Base


v9 : Float
v9 =
    Hueco.grade 9 Mod.Base


font4 : Float
font4 =
    grade 4 Lvl.A Mod.Base


font7c : Float
font7c =
    grade 7 Lvl.C Mod.Base


zero : Grade
zero =
    grade 1 Lvl.A Mod.Base
