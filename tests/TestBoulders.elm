module TestBoulders exposing (..)

import Expect as E
import Fuzz exposing (..)
import Grades.Generic as Grades exposing (br, font, fr, us, vgrade)
import Grades.Levels.ABCPlus as ABCPlus
import Grades.Levels.Mod as Mod
import Grades.Systems.Br as Br
import Grades.Systems.Font as Font
import Grades.Systems.Fr as Fr
import Grades.Systems.Hueco as Hueco
import Grades.Systems.Us as Us
import Grades.TestUtil exposing (seqFromZero)
import Grades.Util exposing (zip)
import Test exposing (..)


expectNumbers : String -> (Float -> String) -> List Float -> ( Int, Int ) -> Test
expectNumbers name toStr values ( start, end ) =
    let
        names =
            List.map toStr values

        named =
            zip names

        expect =
            List.range start end |> List.map toFloat
    in
    test name <| \_ -> E.equalLists (named expect) (named values)


numericRoundtripExt : String -> (a -> Float) -> (Float -> a) -> (a -> String) -> List a -> Test
numericRoundtripExt name toNum fromNum toStr values =
    let
        names =
            List.map toStr values

        named =
            zip names

        nums =
            List.map toNum values

        reconstructed =
            List.map fromNum nums
    in
    test name <| \_ -> E.equalLists (named values) (named reconstructed)


numericRoundtrip : String -> Grades.Generic sys grade -> sys -> List grade -> Test
numericRoundtrip name tt sys value =
    numericRoundtripExt name tt.toLinearScale (tt.fromLinearScale sys) tt.show value


suite : Test
suite =
    describe "Numeric" <|
        [ describe "Conversions" <|
            [ expectNumbers "Hueco" Hueco.show (seqFromZero 20 vgrade) ( -1, 19 )
            , expectNumbers "Font" Font.show (seqFromZero 20 font) ( 1, 21 )
            , expectNumbers "Us" Us.show (seqFromZero 20 us) ( -4, 16 )
            , expectNumbers "Fr" Fr.show (seqFromZero 20 fr) ( 1, 21 )
            , expectNumbers "Br" Br.show (seqFromZero 20 br) ( 1, 21 )
            ]
        , describe "Roundtrips" <|
            [ numericRoundtrip "Font.linearScale" font () (seqFromZero 15 font)
            , numericRoundtrip "Hueco.linearScale" vgrade () (seqFromZero 15 vgrade)
            , numericRoundtrip "Us.linearScale" us () (seqFromZero 20 us)
            , numericRoundtrip "Fr.linearScale" fr () (seqFromZero 20 fr)
            , numericRoundtrip "Br.linearScale" br () (seqFromZero 20 br)
            ]
        ]


regressions : Test
regressions =
    describe "Regressions" <|
        [ test "Font soft" <|
            \_ ->
                let
                    soft =
                        Font.grade 6 ABCPlus.APlus Mod.Soft

                    base =
                        Font.grade 6 ABCPlus.APlus Mod.Base

                    hard =
                        Font.grade 6 ABCPlus.APlus Mod.Hard
                in
                E.equal [ 8.75, 9, 9.25 ] [ soft, base, hard ]
        ]
