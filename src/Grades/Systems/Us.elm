module Grades.Systems.Us exposing
    ( Grade
    , grade
    , parse
    , show
    , zero
    )

import Grades.Levels.ABCD as Lvl
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (usParser)
import Grades.Util exposing (splitNum)
import Parser


{-| Numeric representation of grade progression (YDS)

     1   2   3   4  5.0  5.1  5.2  5.3  ... 5.9  5.10a 5.10b ...
    -4  -3  -2  -1    0    1    2    3  ...   9     10    11 ...

-}
type alias Grade =
    Float


grade : Int -> Lvl.Level -> Mod.Mod -> Float
grade n cat mod =
    if n < 10 then
        toFloat n + Mod.toLinearScale mod

    else
        4 * (toFloat n - 10) + 10 + Mod.toLinearScale mod + 4 * Lvl.toLinearScale cat


split : Float -> { n : Int, cat : Lvl.Level, mod : Mod.Mod }
split x =
    let
        ( n, delta ) =
            splitNum x
    in
    if n < 10 then
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta
        in
        { n = n + incr, cat = Lvl.A, mod = mod }

    else
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 10) // 4 + 10

            ( incr_, lvl ) =
                Lvl.fromIndex <| ((n - 10) |> modBy 4) + incr
        in
        { n = m + incr_, cat = lvl, mod = mod }


show : Grade -> String
show x =
    let
        { n, cat } =
            split x
    in
    if n < 0 then
        String.fromInt (max n -5 + 5)

    else if n < 10 then
        "5." ++ String.fromInt n

    else
        "5." ++ String.fromInt n ++ Lvl.show cat


parse : String -> Maybe Grade
parse st =
    st
        |> Parser.run (usParser grade)
        |> Result.toMaybe


zero : Grade
zero =
    grade -4 Lvl.A Mod.Base
