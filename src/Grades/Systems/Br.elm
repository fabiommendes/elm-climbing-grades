module Grades.Systems.Br exposing
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

import Grades.Levels.ABC as Lvl
import Grades.Levels.Mod as Mod
import Grades.Parser exposing (brParser)
import Grades.Systems.Common exposing (..)
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


fromLinearScale : Float -> Grade
fromLinearScale _ =
    zero


toLinearScale : Grade -> Float
toLinearScale _ =
    0.0


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
