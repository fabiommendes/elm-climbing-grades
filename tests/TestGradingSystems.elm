module TestGradingSystems exposing (..)

import Expect as E
import Fuzz exposing (..)
import Grades.Bouldering exposing (System(..))
import Grades.Climbing exposing (System(..))
import Grades.Generic as Grades
import Grades.Levels.Mod as Mod
import Grades.Systems.Hueco as Hueco
import Grades.TestUtil exposing (fullSeqFromZero, seqFromZero)
import Grades.Util exposing (iterate, seqDecreases)
import Test exposing (..)


huecoToFont : String
huecoToFont =
    """VB - 2
V0 - 4
V1 - 4b/+
V2 - 5+
V3 - 6a+ soft
V4 - 6b hard
V5 - 6c soft
V6 - 6c+ hard
V7 - 7a+
V8 - 7b/+
V9 - 7c
V10 - 7c+
V11 - 8a
V12 - 8a+
V13 - 8b
V14 - 8b+
V15 - 8c
V16 - 8c+
V17 - 9a
V18 - 9a+
V19 - 9b"""


fontToHueco : String
fontToHueco =
    """2 - VB
3 - VB/0
4 - V0
4+ - V1-
5 - V1+
5+ - V2
6a - V2/3
6a+ - V3+
6b - V4-
6b+ - V4/5
6c - V5+
6c+ - V6-
7a - V6/7
7a+ - V7
7b - V8-
7b+ - V8+
7c - V9
7c+ - V10
8a - V11
8a+ - V12
8b - V13"""


usTable : String
usTable =
    """1 - I BR - 1
2 - Isup soft BR - 2 soft
3 - Isup hard BR - 2/3
4 - II BR - 3 hard
5.0 - IIsup soft BR - 4a soft
5.1 - IIsup/III BR - 4a/b
5.2 - III BR - 4b hard
5.3 - IIIsup soft BR - 4c
5.4 - IIIsup/IV BR - 5a soft
5.5 - IV hard BR - 5a/+
5.6 - IVsup soft BR - 5b hard
5.7 - IVsup/V BR - 5c soft
5.8 - V hard BR - 5c/+
5.9 - Vsup soft BR - 6a hard
5.10a - Vsup/VI BR - 6a+
5.10b - VI hard BR - 6b soft
5.10c - VIsup BR - 6b/+
5.10d - 6sup/7a BR - 6b+ hard
5.11a - 7a hard BR - 6c soft
5.11b - 7b BR - 6c/+
5.11c - 7c soft BR - 6c+ hard"""


frTable : String
frTable =
    """1 - 1 - I BR
2 - 2 - Isup BR
3 - 4 - II BR
4a - 5.0 - IIsup soft BR
4b - 5.1 - III soft BR
4c - 5.3 - IIIsup soft BR
5a - 5.4 - IV soft BR
5b - 5.6 - IVsup soft BR
5c - 5.7 - V soft BR
6a - 5.8 - V/sup BR
6a+ - 5.10a - Vsup/VI BR
6b - 5.10b - VI/sup BR
6b+ - 5.10d - 6sup/7a BR
6c - 5.11a - 7a/b BR
6c+ - 5.11b - 7b hard BR
7a - 5.11d - 7c hard BR
7a+ - 5.12a - 8a BR
7b - 5.12b - 8b BR
7b+ - 5.12c - 8c BR
7c - 5.12d - 9a BR
7c+ - 5.13a - 9b BR"""


brTable : String
brTable =
    """I BR - 1 - 1
Isup BR - 2 - 2
II BR - 4 - 3
IIsup BR - 5.0 - 4a hard
III BR - 5.2 - 4b hard
IIIsup BR - 5.3 - 4c hard
IV BR - 5.5 - 5a hard
IVsup BR - 5.6 - 5b hard
V BR - 5.8 - 5c/+
Vsup BR - 5.9 - 6a/+
VI BR - 5.10b - 6a+/b
VIsup BR - 5.10c - 6b/+
7a BR - 5.10d - 6b+/c
7b BR - 5.11b - 6c/+
7c BR - 5.11c - 7a soft
8a BR - 5.12a - 7a+
8b BR - 5.12b - 7b
8c BR - 5.12c - 7b+
9a BR - 5.12d - 7c
9b BR - 5.13a - 7c+
9c BR - 5.13b - 8a"""


conversionTablePair : Grades.Generic a grade -> a -> a -> Int -> List String
conversionTablePair tt sys1 sys2 n =
    iterate tt.next (tt.zero |> tt.to sys1 |> tt.simplify) n
        |> List.map (\g -> tt.show g ++ " - " ++ tt.showAs sys2 g)


conversionTableTriple : Grades.Generic a grade -> a -> a -> a -> Int -> List String
conversionTableTriple tt sys1 sys2 sys3 n =
    iterate tt.next (tt.zero |> tt.to sys1 |> tt.simplify) n
        |> List.map (\g -> tt.show g ++ " - " ++ tt.showAs sys2 g ++ " - " ++ tt.showAs sys3 g)


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


conversion : Test
conversion =
    describe "Conversion tables"
        [ test "Hueco -> Font" <|
            \_ ->
                conversionTablePair Grades.boulder VGrade Fontainbleau 20
                    |> E.equalLists (huecoToFont |> String.split "\n")
        , test "Font -> Hueco" <|
            \_ ->
                conversionTablePair Grades.boulder Fontainbleau VGrade 20
                    |> E.equalLists (fontToHueco |> String.split "\n")
        , test "Us Br Fr" <|
            \_ ->
                conversionTableTriple Grades.climb US BR FR 20
                    |> E.equalLists (usTable |> String.split "\n")
        , test "Br Fr Us" <|
            \_ ->
                conversionTableTriple Grades.climb BR US FR 20
                    |> E.equalLists (brTable |> String.split "\n")
        , test "Fr Us Br" <|
            \_ ->
                conversionTableTriple Grades.climb FR US BR 20
                    |> E.equalLists (frTable |> String.split "\n")
        ]
