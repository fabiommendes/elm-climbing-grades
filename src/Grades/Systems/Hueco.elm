module Grades.Systems.Hueco exposing
    ( Grade
    , fromLinearScale
    , next
    , order
    , parse
    , prev
    , show
    , simplify
    , toLinearScale
    , zero, withMod
    )

import Grades.Levels.Mod as Mod
import Grades.Parser exposing (vParser)
import Parser exposing (..)


type alias Grade =
    { n : Int, mod : Mod.Mod }


show : Grade -> String
show { n, mod } =
    let
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
        |> run (vParser Grade)
        |> Result.toMaybe


simplify : Grade -> Grade
simplify { n } =
    Grade n Mod.Base


withMod : Mod.Mod -> Grade -> Grade
withMod mod { n } =
    Grade n mod


{-| Convert to the floating point universal scale

This is useful to convert to different grading systems or
saving in a database.

    font =
        grade
            |> Hueco.toLinearScale
            |> Font.fromLinearScale

-}
toLinearScale : Grade -> Float
toLinearScale { n, mod } =
    toFloat n + Mod.toLinearScale mod


fromLinearScale : Float -> Grade
fromLinearScale x =
    let
        n =
            floor x

        err =
            x - toFloat n

        ( m, mod ) =
            Mod.fromLinearScale n err
    in
    Grade m mod


zero : Grade
zero =
    Grade -1 Mod.Base


next : Grade -> Grade
next { n, mod } =
    Grade (n + 1) mod


prev : Grade -> Grade
prev { n, mod } =
    Grade (n - 1) mod


order : Grade -> Grade -> Order
order g1 g2 =
    if g1.n > g2.n then
        GT

    else if g1.n == g1.n then
        Mod.order g1.mod g2.mod

    else
        LT
