module Grades.Levels.Mod exposing (Mod(..), base, fromFloat, fromLinearScale, halfwayNext, hard, order, showModSoftHard, showPlusMinus, soft, toBase, toLinearScale, toMod)

{-| Sub-levels and categories inside a given grade class.

Example scale

    ...
    [2.625, 2.875) - 4 soft
    [2.875, 3.125) - 3 base
    [3.125, 3.375) - 3 hard
    [3.375, 3.625) - 3 halfwaynext
    [3.625, 3.875) - 4 soft
    [3.875, 4.125) - 4 base
    [4.125, 4.375) - 4 hard
    [4.375, 4.625) - 4 halfwaynext
    ...

-}

import Grades.Util exposing (splitNum)


{-| Difficulty modifier
-}
type Mod
    = Base
    | Soft
    | Hard
    | HalfwayNext


toBase : Float -> Float
toBase x =
    floor (x + 0.375) |> toFloat


toMod : Mod -> Float -> Float
toMod mod x =
    toBase x + toLinearScale mod


soft : Int -> Float
soft n =
    toFloat n - 0.365


base : Int -> Float
base n =
    toFloat n - 0.125


hard : Int -> Float
hard n =
    toFloat n + 0.125


halfwayNext : Int -> Float
halfwayNext n =
    toFloat n + 0.365


order : Mod -> Mod -> Order
order a b =
    compare (toLinearScale a) (toLinearScale b)


showExt : ( String, String ) -> Mod -> Maybe String
showExt mods mod =
    case mod of
        Base ->
            Just ""

        Soft ->
            Just <| Tuple.first mods

        Hard ->
            Just <| Tuple.second mods

        HalfwayNext ->
            Nothing


showPlusMinus : Mod -> Maybe String
showPlusMinus =
    showExt ( "-", "+" )


showModSoftHard : Mod -> Maybe String
showModSoftHard =
    showExt ( " soft", " hard" )


toLinearScale : Mod -> Float
toLinearScale mod =
    case mod of
        Soft ->
            -0.25

        Base ->
            0.0

        Hard ->
            0.25

        HalfwayNext ->
            0.5


fromFloat : Float -> ( Int, Mod )
fromFloat x =
    let
        ( n, delta ) =
            splitNum x
    in
    if delta < 0.125 then
        ( n, Base )

    else if delta < 0.375 then
        ( n, Hard )

    else if delta < 0.625 then
        ( n, HalfwayNext )

    else if delta < 0.875 then
        ( n + 1, Soft )

    else
        ( n + 1, Base )


fromLinearScale : number -> Float -> ( number, Mod )
fromLinearScale n x =
    if x < 0 then
        fromLinearScale (n - 1) (x + 1)

    else if x > 1 then
        fromLinearScale (n + 1) (x - 1)

    else if x < 0.125 then
        ( n, Base )

    else if x < 0.375 then
        ( n, Hard )

    else if x < 0.625 then
        ( n, HalfwayNext )

    else if x < 0.875 then
        ( n + 1, Soft )

    else
        ( n + 1, Base )
