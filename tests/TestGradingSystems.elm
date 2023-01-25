module TestGradingSystems exposing (..)

import Expect as E
import Fuzz exposing (..)
import Grades.Generic as Grades
import Test exposing (..)


sequenceFromZero : Grades.Generic a b -> String -> String -> Test
sequenceFromZero tt descr spec =
    let
        seq =
            String.split " " spec
    in
    test descr <|
        \_ ->
            E.equal
                (iterate tt.next tt.zero (List.length seq - 1)
                    |> List.map tt.show
                )
                seq


iterate : (a -> a) -> a -> Int -> List a
iterate f x n =
    if n <= 0 then
        [x]

    else
        x :: iterate f (f x) (n - 1)


suite : Test
suite =
    describe "zero/next sequences"
        [ sequenceFromZero Grades.vgrade "Hueco (boulder)" "VB V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 V10"
        , sequenceFromZero Grades.font "Font (boulder)" "1 2 3 4 4+ 5 5+ 6a 6a+ 6b 6b+ 6c 6c+ 7a"
        , sequenceFromZero Grades.us "US" "1 2 3 4 5.0 5.1 5.2 5.3 5.4 5.5 5.6 5.7 5.8 5.9 5.10a 5.10b 5.10c 5.10d 5.11a"
        , sequenceFromZero Grades.fr "FR" "1 2 3 4a 4b 4c 5a 5b 5c 6a 6a+ 6b 6b+ 6c 6c+ 7a"
        , sequenceFromZero Grades.br "BR" "I Isup II IIsup III IIIsup IV IVsup V Vsup VI VIsup 7a 7b 7c 8a"
        ]



-- [ describe "Hueco.parse"
--     (let
--         example st n =
--             test ("parse " ++ st) <| \_ -> parse st |> E.equal (v n)
--         invalid st =
--             test ("parse " ++ st) <| \_ -> parse st |> E.equal Nothing
--      in
--      [ -- Vermin/Hueco scale
--        example "VB" -1.0
--      , example "V1" 1.0
--      , example "v2" 2.0
--      , example "v10" 10.0
--      , example "V11" 11.0
--      , example "V1+" 1.2
--      , example "V1/2" 1.5
--      , example "V1-" 0.8
--      , invalid "V1/V2"
--      , invalid "1"
--      , invalid "V-1"
--      , invalid "V00"
--      , invalid "V1/3"
--      ]
--     )
-- ]
