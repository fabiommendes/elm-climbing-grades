module Grades.Climbing exposing
    ( Grade, System(..)
    , show, showAs, parse, parseAs
    , simplify, withMod, next, prev, to
    , zero, compare, toLinearScale, fromLinearScale
    )

{-| Climbing grades representation and conversion


## Reference

Conversions are based on the table

  - <https://en.wikipedia.org/wiki/Grade_(climbing)>


## Types

@docs Grade, System


## Parsing and rendering

@docs show, showAs, parse, parseAs


## Transforms

@docs simplify, withMod, next, prev, to


## Ordering and comparisons

@docs zero, compare, toLinearScale, fromLinearScale

-}

import Grades.Levels.Mod as Mod
import Grades.Systems.Br as Br
import Grades.Systems.Fr as Fr
import Grades.Systems.Us as Us


{-| A climbing grade
-}
type Grade
    = Us Us.Grade
    | Fr Fr.Grade
    | Br Br.Grade


{-| A climbing grading system
-}
type System
    = US
    | FR
    | BR


{-| Render grade as a string
-}
show : Grade -> String
show =
    unwrap
        ( Us.show
        , Fr.show
        , Br.show >> (\x -> x ++ " BR")
        )


{-| Render grade using some grading system
-}
showAs : System -> Grade -> String
showAs system =
    to system >> show


{-| Parse grade string
-}
parse : String -> Maybe Grade
parse st =
    if String.endsWith "BR" st then
        st
            |> String.dropRight 2
            |> String.trim
            |> Br.parse
            |> Maybe.map Br

    else if String.endsWith "US" st then
        st
            |> String.dropRight 2
            |> String.trim
            |> Us.parse
            |> Maybe.map Us

    else if String.endsWith "FR" st then
        st
            |> String.dropRight 2
            |> String.trim
            |> Fr.parse
            |> Maybe.map Fr

    else if String.startsWith "5." st then
        Us.parse st |> Maybe.map Us

    else
        Fr.parse st |> Maybe.map Fr


{-| Parse grade string using Hueco V-grade system
-}
parseAs : System -> String -> Maybe Grade
parseAs system =
    case system of
        US ->
            Us.parse >> Maybe.map Us

        FR ->
            Fr.parse >> Maybe.map Fr

        BR ->
            Br.parse >> Maybe.map Br


{-| Remove modifiers (soft, hard, etc) from grade
-}
simplify : Grade -> Grade
simplify =
    map Mod.toBase


{-| Set modifiers (soft, hard, etc)
-}
withMod : Mod.Mod -> Grade -> Grade
withMod mod =
    map (Mod.toMod mod)


{-| Construct grade from numeric value
-}
fromLinearScale : System -> Float -> Grade
fromLinearScale system x =
    case system of
        US ->
            Us x

        FR ->
            Fr (Fr.fromLinearScale x)

        BR ->
            Br (Br.fromLinearScale x)


{-| Convert grade to numeric value for easy comparison
-}
toLinearScale : Grade -> Float
toLinearScale =
    unwrap ( identity, Fr.toLinearScale, Br.toLinearScale )


{-| Smallest possible grade
-}
zero : Grade
zero =
    Us Us.zero


{-| Next discrete grade in the current grading system.

This ignores modifiers and some intermediate levels.

-}
next : Grade -> Grade
next =
    map ((+) 1)


{-| Previous discrete grade in the current grading system.

This ignores modifiers and some intermediate levels.

-}
prev : Grade -> Grade
prev =
    map ((+) -1)


{-| Compare two grades and return an ordering relation
-}
compare : Grade -> Grade -> Order
compare a b =
    case ( a, b ) of
        ( Us x, Us y ) ->
            Basics.compare x y

        ( Fr x, Fr y ) ->
            Basics.compare x y

        ( Br x, Br y ) ->
            Basics.compare x y

        _ ->
            Basics.compare (toLinearScale a) (toLinearScale b)


{-| Convert grade to the given system
-}
to : System -> Grade -> Grade
to system grade =
    case ( system, grade ) of
        ( US, Us _ ) ->
            grade

        ( FR, Fr _ ) ->
            grade

        ( BR, Br _ ) ->
            grade

        _ ->
            toLinearScale grade |> fromLinearScale system


map : (Float -> Float) -> Grade -> Grade
map f grade =
    case grade of
        Us x ->
            Us (f x)

        Fr x ->
            Fr (f x)

        Br x ->
            Br (f x)


unwrap : ( Float -> a, Float -> a, Float -> a ) -> Grade -> a
unwrap ( f, g, h ) grade =
    case grade of
        Us x ->
            f x

        Fr x ->
            g x

        Br x ->
            h x
