module Grades.Levels.ABC exposing (Level(..), next, prev, show, showHalfway, toLinearScale, fromIndex)


type Level
    = A
    | B
    | C


next : Level -> Maybe Level
next lvl =
    case lvl of
        A ->
            Just B

        B ->
            Just C

        C ->
            Nothing


prev : Level -> Maybe Level
prev lvl =
    case lvl of
        A ->
            Nothing

        B ->
            Just A

        C ->
            Just B


show : Level -> String
show lvl =
    case lvl of
        A ->
            "a"

        B ->
            "b"

        C ->
            "c"


showHalfway : Level -> Maybe String
showHalfway lvl =
    case lvl of
        A ->
            Just "a/b"

        B ->
            Just "b/c"

        C ->
            Nothing


toLinearScale : Level -> Float
toLinearScale lvl =
    case lvl of
        A ->
            0 / 3

        B ->
            1 / 3

        C ->
            2 / 3


fromIndex : Int -> ( Int, Level )
fromIndex n =
    case n of
        0 ->
            ( 0, A )

        1 ->
            ( 0, B )

        2 ->
            ( 0, C )

        _ ->
            if n < 0 then
                fromIndex (n + 3) |> Tuple.mapFirst ((+) -1)

            else
                fromIndex (n - 3) |> Tuple.mapFirst ((+) 1)
