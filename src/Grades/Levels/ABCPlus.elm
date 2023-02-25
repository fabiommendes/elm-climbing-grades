module Grades.Levels.ABCPlus exposing (Level(..), fromIndex, next, prev, show, showHalfway, toABC, toLinearScale, fromABC)

import Grades.Levels.ABC as ABC


type Level
    = A
    | APlus
    | B
    | BPlus
    | C
    | CPlus


next : Level -> Maybe Level
next lvl =
    case lvl of
        A ->
            Just APlus

        APlus ->
            Just B

        B ->
            Just BPlus

        BPlus ->
            Just C

        C ->
            Just CPlus

        CPlus ->
            Nothing


prev : Level -> Maybe Level
prev lvl =
    case lvl of
        A ->
            Nothing

        APlus ->
            Just A

        B ->
            Just APlus

        BPlus ->
            Just B

        C ->
            Just BPlus

        CPlus ->
            Just C


toABC : Level -> ABC.Level
toABC lvl =
    case lvl of
        A ->
            ABC.A

        APlus ->
            ABC.A

        B ->
            ABC.B

        BPlus ->
            ABC.B

        C ->
            ABC.C

        CPlus ->
            ABC.C


fromABC : ABC.Level -> Level
fromABC lvl =
    case lvl of
        ABC.A ->
            A

        ABC.B ->
            B

        ABC.C ->
            C


show : Level -> String
show lvl =
    case lvl of
        A ->
            "a"

        APlus ->
            "a+"

        B ->
            "b"

        BPlus ->
            "b+"

        C ->
            "c"

        CPlus ->
            "c+"


showHalfway : Level -> Maybe String
showHalfway cat =
    case cat of
        A ->
            Just "a/+"

        APlus ->
            Just "a+/b"

        B ->
            Just "b/+"

        BPlus ->
            Just "b+/c"

        C ->
            Just "c/+"

        CPlus ->
            Nothing


toLinearScale : Level -> Float
toLinearScale cat =
    case cat of
        A ->
            0 / 6

        APlus ->
            1 / 6

        B ->
            2 / 6

        BPlus ->
            3 / 6

        C ->
            4 / 6

        CPlus ->
            5 / 6


fromIndex : Int -> ( Int, Level )
fromIndex n =
    case n of
        0 ->
            ( 0, A )

        1 ->
            ( 0, APlus )

        2 ->
            ( 0, B )

        3 ->
            ( 0, BPlus )

        4 ->
            ( 0, C )

        5 ->
            ( 0, CPlus )

        _ ->
            if n < 0 then
                fromIndex (n + 6) |> Tuple.mapFirst ((+) -1)

            else
                fromIndex (n - 6) |> Tuple.mapFirst ((+) 1)
