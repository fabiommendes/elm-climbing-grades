module Grades.Systems.Hueco exposing
    ( Grade
    , grade
    , parse
    , show
    , zero
    )

import Grades.Levels.Mod as Mod
import Grades.Parser exposing (vParser)
import Parser exposing (..)


{-| Numeric representation of grade progression

    Progression: VB  V0  V1  V2  V3...
                 -1   0   1   2   3...

-}
type alias Grade =
    Float


grade : Int -> Mod.Mod -> Float
grade n mod =
    toFloat n + Mod.toLinearScale mod


show : Grade -> String
show x =
    let
        ( n, mod ) =
            Mod.fromFloat x

        base =
            if n < 0 then
                "VB"

            else
                "V" ++ String.fromInt n
    in
    case mod of
        Mod.Base ->
            base

        Mod.Soft ->
            base ++ "-"

        Mod.Hard ->
            base ++ "+"

        Mod.HalfwayNext ->
            base ++ "/" ++ String.fromInt (n + 1)


parse : String -> Maybe Grade
parse st =
    st
        |> run (vParser grade)
        |> Result.toMaybe


zero : Grade
zero =
    -1
