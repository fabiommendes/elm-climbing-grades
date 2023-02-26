module TestGradingSystems exposing (..)

import Expect as E
import Fuzz exposing (..)
import Grades.Generic as Grades
import Grades.Levels.Mod as Mod
import Grades.Systems.Hueco as Hueco
import Grades.TestUtil exposing (fullSeqFromZero, seqFromZero)
import Grades.Util exposing (iterate, seqDecreases)
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


suite : Test
suite =
    describe "zero/next sequences"
        [ sequenceFromZero Grades.vgrade "Hueco (boulder)" "VB V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 V10"
        , sequenceFromZero Grades.font "Font (boulder)" "1 2 3 4 4+ 5 5+ 6a 6a+ 6b 6b+ 6c 6c+ 7a"
        , sequenceFromZero Grades.us "US" "1 2 3 4 5.0 5.1 5.2 5.3 5.4 5.5 5.6 5.7 5.8 5.9 5.10a 5.10b 5.10c 5.10d 5.11a"
        , sequenceFromZero Grades.fr "FR" "1 2 3 4a 4b 4c 5a 5b 5c 6a 6a+ 6b 6b+ 6c 6c+ 7a"
        , sequenceFromZero Grades.br "BR" "I Isup II IIsup III IIIsup IV IVsup V Vsup VI VIsup 7a 7b 7c 8a"
        ]


parsing : Test
parsing =
    describe "Hueco.parse"
        (let
            parse =
                Hueco.parse

            example st n mod =
                test ("parse " ++ st) <| \_ -> parse st |> E.equal (Just <| Hueco.grade n mod)

            invalid st =
                test ("parse " ++ st) <| \_ -> parse st |> E.equal Nothing
         in
         [ -- Vermin/Hueco scale
           example "VB" -1 Mod.Base
         , example "V1" 1 Mod.Base
         , example "V2" 2 Mod.Base
         , example "V10" 10 Mod.Base
         , example "V11" 11 Mod.Base
         , example "V1+" 1 Mod.Hard
         , example "V1/2" 1 Mod.HalfwayNext
         , example "V1-" 1 Mod.Soft
         , invalid "V1/V2"
         , invalid "1"
         , invalid "V-1"
         , invalid "V00"
         , invalid "V1/3"
         ]
        )


sequences : Test
sequences =
    describe "strictly increasing sequences"
        [ test "Hueco" <| \_ -> seqDecreases Grades.vgrade.toLinearScale (seqFromZero 20 Grades.vgrade) |> E.equal []
        ]


linearScale : Test
linearScale =
    let
        tests : String -> sys -> Grades.Generic sys grade -> List Test
        tests name sys mod =
            fullSeqFromZero 20 mod
                |> List.map
                    (\g ->
                        let
                            n =
                                mod.toLinearScale g
                        in
                        test (name ++ " " ++ mod.show g) <|
                            \_ -> ( n, mod.fromLinearScale sys n ) |> E.equal ( n, g )
                    )
    in
    describe "linear scale roundtrip" <|
        List.concat
            [ tests "Hueco" () Grades.vgrade
            , tests "Font" () Grades.font
            ]
