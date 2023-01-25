module Grades.Bouldering exposing
    ( Grade, System(..)
    , show, showAs, parse, parseAs
    , simplify, next, prev, to
    , zero, compare, toLinearScale, fromLinearScale
    )

{-| Bouldering grades representation and conversion


## Reference

Conversions are based on the table

  - <https://en.wikipedia.org/wiki/Grade_(bouldering)>


## Types

@docs Grade, System


## Parsing and rendering

@docs show, showAs, parse, parseAs


## Transforms

@docs simplify, next, prev, to


## Ordering and comparisons

@docs zero, compare, toLinearScale, fromLinearScale

-}

import Grades.Systems.Font as Font
import Grades.Systems.Hueco as Hueco


{-| Represents a bouldering grade
-}
type Grade
    = Hueco Hueco.Grade
    | Font Font.Grade


{-| A bouldering grade system
-}
type System
    = VGrade
    | Fontainbleau


{-| Render grade as a string
-}
show : Grade -> String
show =
    map ( Hueco.show, Font.show )


{-| Render grade using some grading system
-}
showAs : System -> Grade -> String
showAs system =
    to system >> show


{-| Parse grade string
-}
parse : String -> Maybe Grade
parse st =
    if String.startsWith "V" st then
        Hueco.parse st |> Maybe.map Hueco

    else
        Font.parse st |> Maybe.map Font


{-| Parse grade string using Hueco V-grade system
-}
parseAs : System -> String -> Maybe Grade
parseAs system =
    case system of
        VGrade ->
            Hueco.parse >> Maybe.map Hueco

        Fontainbleau ->
            Font.parse >> Maybe.map Font


{-| Compare two grades and return an ordering relation
-}
compare : Grade -> Grade -> Order
compare a b =
    case ( a, b ) of
        ( Hueco x, Hueco y ) ->
            Hueco.order x y

        ( Font x, Font y ) ->
            Font.order x y

        _ ->
            Basics.compare (toLinearScale a) (toLinearScale b)


{-| Construct grade from numeric value
-}
fromLinearScale : System -> Float -> Grade
fromLinearScale system x =
    case system of
        VGrade ->
            Hueco (Hueco.fromLinearScale x)

        Fontainbleau ->
            Font (Font.fromLinearScale x)


{-| Convert grade to numeric value for easy comparison
-}
toLinearScale : Grade -> Float
toLinearScale =
    map ( Hueco.toLinearScale, Font.toLinearScale )


{-| Smallest possible grade
-}
zero : Grade
zero =
    Hueco Hueco.zero


{-| Next discrete grade in the current grading system.

This ignores modifiers and some intermediate levels.

-}
next : Grade -> Grade
next =
    flatmap ( Hueco.next, Font.next )


{-| Previous discrete grade in the current grading system.

This ignores modifiers and some intermediate levels.

-}
prev : Grade -> Grade
prev =
    flatmap ( Hueco.prev, Font.prev )


{-| Remove modifiers (soft, hard, etc) from grade
-}
simplify : Grade -> Grade
simplify =
    flatmap ( Hueco.simplify, Font.simplify )


{-| Convert grade to the given system
-}
to : System -> Grade -> Grade
to system grade =
    case ( system, grade ) of
        ( VGrade, Hueco _ ) ->
            grade

        ( Fontainbleau, Font _ ) ->
            grade

        _ ->
            toLinearScale grade |> fromLinearScale system


map : ( Hueco.Grade -> a, Font.Grade -> a ) -> Grade -> a
map ( f, g ) grade =
    case grade of
        Hueco x ->
            f x

        Font x ->
            g x


flatmap : ( Hueco.Grade -> Hueco.Grade, Font.Grade -> Font.Grade ) -> Grade -> Grade
flatmap ( f, g ) grade =
    case grade of
        Hueco x ->
            Hueco (f x)

        Font x ->
            Font (g x)
