module Grades.Systems.Br exposing
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

import Grades.Levels.ABC as Lvl
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (brParser)
import Grades.Systems.Common exposing (..)
import Grades.Systems.Us as Us
import Grades.Util exposing (piecewise, splitNum)
import Parser
import Roman exposing (toRoman)


type alias Grade =
    { n : Int
    , cat : Lvl.Level
    , mod : Mod.Mod
    }


show : Grade -> String
show grade =
    let
        { n, cat, mod } =
            grade
    in
    if n == 6 && cat /= Lvl.A && mod == Mod.HalfwayNext then
        -- Special cases
        "6sup/7a"

    else if n <= 6 && mod == Mod.HalfwayNext && cat == Lvl.A then
        -- Easy grades (I to VIsup) treat Lvl.B3 and Lvl.C3 as
        -- equivalent and correspond to the "sup" modifier
        toRoman n ++ "/sup"

    else if n <= 6 then
        case Mod.showModSoftHard mod of
            Just suffix ->
                toRoman n ++ select (cat == Lvl.A) "" "sup" ++ suffix

            Nothing ->
                toRoman n ++ "sup/" ++ toRoman (n + 1)

    else
        -- Grades such as 7a, 9c, etc are more regular
        case Mod.showModSoftHard mod of
            Just suffix ->
                String.fromInt n ++ Lvl.show cat ++ suffix

            Nothing ->
                case Lvl.showHalfway cat of
                    Just lvl ->
                        String.fromInt n ++ lvl

                    Nothing ->
                        show (grade |> simplify) ++ "/" ++ show (grade |> simplify |> next)


parse : String -> Maybe Grade
parse st =
    st
        |> Parser.run (brParser Grade)
        |> Result.toMaybe


simplify : Grade -> Grade
simplify { n, cat } =
    Grade n cat Mod.Base


withMod : Mod.Mod -> Grade -> Grade
withMod mod { n, cat } =
    Grade n cat mod


toLinearScale : Grade -> Float
toLinearScale =
    toNum >> piecewise 0.75 1.0 ( br1, us3 ) [] ( br8a, us512a )


fromLinearScale : Float -> Grade
fromLinearScale =
    piecewise (1 / 0.75) 1.0 ( us3, br1 ) [] ( us512a, br8a ) >> fromNum


us3 : Float
us3 =
    Us.toLinearScale (Us.parse "3" |> Maybe.withDefault Us.zero)


us512a : Float
us512a =
    Us.toLinearScale (Us.parse "5.12a" |> Maybe.withDefault Us.zero)


br8a : Float
br8a =
    toNum (Grade 8 Lvl.A Mod.Base)


br1 : Float
br1 =
    toNum (Grade 1 Lvl.A Mod.Base)


toNum : Grade -> Float
toNum { n, cat, mod } =
    -- Progression: I Isup II IIsup III IIIsup IV IVsup  V  Vsup  VI  VIsup  7a  7b  7c ...
    --              1   2   3    4    5     6   7    8   9   10   11    12   13  14  15 ...
    if n <= 6 then
        2 * toFloat n - 1 + 3 * Lvl.toLinearScale cat + Mod.toLinearScale mod

    else
        13 + 3 * toFloat (n - 7) + 3 * Lvl.toLinearScale cat + Mod.toLinearScale mod


fromNum : Float -> Grade
fromNum x =
    let
        ( n, delta ) =
            splitNum x
    in
    if n < 13 then
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 1) // 2 + 1

            ( lvl, incr_ ) =
                case ( n - 1 + incr |> modBy 2, incr ) of
                    ( 0, 1 ) ->
                        ( Lvl.A, 1 )

                    ( 0, _ ) ->
                        ( Lvl.A, 0 )

                    ( 1, _ ) ->
                        ( Lvl.B, 0 )

                    _ ->
                        ( Lvl.A, 0 )
        in
        Grade (m + incr_) lvl mod

    else
        let
            ( incr, mod ) =
                Mod.fromLinearScale 0 delta

            m =
                (n - 13) // 3 + 7

            ( incr_, lvl ) =
                Lvl.fromIndex <| ((n - 13) |> modBy 3) + incr
        in
        Grade (m + incr_) lvl mod


zero : Grade
zero =
    { n = 1, cat = Lvl.A, mod = Mod.Base }


next : Grade -> Grade
next { n, cat, mod } =
    if n <= 6 && cat == Lvl.A then
        Grade n Lvl.B mod

    else if n <= 6 then
        Grade (n + 1) Lvl.A mod

    else
        case Lvl.next cat of
            Just lvl ->
                Grade n lvl mod

            Nothing ->
                Grade (n + 1) Lvl.A mod


prev : Grade -> Grade
prev { n, cat, mod } =
    if n <= 6 && cat /= Lvl.A then
        Grade n Lvl.A mod

    else if n <= 6 then
        Grade (n - 1) Lvl.B mod

    else
        case Lvl.prev cat of
            Just lvl ->
                Grade n lvl mod

            Nothing ->
                Grade (n - 1) Lvl.C mod


order : Grade -> Grade -> Order
order a b =
    let
        toTuple { n, cat, mod } =
            ( n, Lvl.toLinearScale cat, Mod.toLinearScale mod )
    in
    compare (toTuple a) (toTuple b)
