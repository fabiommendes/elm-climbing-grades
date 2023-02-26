module Grades.Util exposing (..)


seqDecreases : (a -> Float) -> List a -> List ( Float, a )
seqDecreases toFloat seq =
    let
        do ( n, x ) elems =
            case elems of
                y :: rest ->
                    let
                        m =
                            toFloat y
                    in
                    if m <= n then
                        ( n, x ) :: ( m, y ) :: do ( m, y ) rest

                    else
                        do ( m, y ) rest

                _ ->
                    []
    in
    case seq of
        x :: xs ->
            do ( toFloat x, x ) xs

        _ ->
            []


seqUniform : (a -> Float) -> List a -> List ( Float, a )
seqUniform toFloat seq =
    let
        do ( delta, n, x ) elems =
            case elems of
                y :: rest ->
                    let
                        m =
                            toFloat y
                    in
                    if abs ((m - n) - delta) > 0.01 then
                        ( delta, n, x ) :: ( m - n, m, y ) :: do ( m - n, m, y ) rest

                    else
                        do ( m - n, m, y ) rest

                _ ->
                    []
    in
    case seq of
        x :: y :: xs ->
            do ( toFloat y - toFloat x, toFloat y, y ) xs
                |> List.map (\( num, _, elem ) -> ( num, elem ))

        _ ->
            []


iterate : (a -> a) -> a -> Int -> List a
iterate f x n =
    if n <= 0 then
        [ x ]

    else
        x :: iterate f (f x) (n - 1)


flip : (c -> b -> a) -> b -> c -> a
flip f x y =
    f y x


zip : List a -> List b -> List ( a, b )
zip fst snd =
    case ( fst, snd ) of
        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys

        _ ->
            []


piecewise : Float -> Float -> ( Float, Float ) -> List ( Float, Float ) -> ( Float, Float ) -> Float -> Float
piecewise preSlope postSlope ( x0, y0 ) parts ( x1, y1 ) x =
    if x < x0 then
        (x - x0) * preSlope + y0

    else if x > x1 then
        (x - x1) * postSlope + y1

    else
        case parts of
            [] ->
                (x - x0) * ((y1 - y0) / (x1 - x0)) + y0

            ( xx, yy ) :: rest ->
                if x > xx then
                    piecewise ((yy - y0) / (xx - x0)) postSlope ( xx, yy ) rest ( x1, y1 ) x

                else
                    piecewise preSlope ((y1 - yy) / (x1 - xx)) ( x0, y0 ) rest ( xx, yy ) x


trunc : Float -> Float
trunc x =
    toFloat (Basics.round (x * 100)) / 100


splitNum : Float -> ( Int, Float )
splitNum x =
    let
        n =
            floor x
    in
    ( n, x - toFloat n )


normalizeNum : Float -> Float
normalizeNum =
    (*) 10000 >> round >> toFloat >> flip (/) 10000
