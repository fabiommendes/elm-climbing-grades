module TestBoulders exposing (..)

import Expect as E
import Fuzz exposing (..)
import Grades.Generic as Grades exposing (br, font, fr, us, vgrade)
import Grades.Levels.ABCPlus as ABCPlus
import Grades.Levels.Mod as Mod
import Grades.Systems.Br as Br
import Grades.Systems.Font as Font
import Grades.Systems.Fr as Fr
import Grades.TestUtil exposing (seqFromZero)
import Grades.Util exposing (trunc, zip)
import Test exposing (..)


expectNumbers : String -> (a -> Float) -> (a -> String) -> List a -> List Float -> Test
expectNumbers name toNum toStr values expect =
    let
        names =
            List.map toStr values

        named =
            zip names
    in
    test name <| \_ -> E.equalLists (named expect) (named <| List.map (toNum >> trunc) values)


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
            [ expectNumbers "Hueco.toLinearScale" vgrade.toLinearScale vgrade.show (seqFromZero 11 vgrade) [ -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
            , expectNumbers "Font.toLinearScale" Font.toLinearScale Font.show (seqFromZero 20 font) [ -1.5, -1, -0.5, 0, 0.64, 1.29, 1.93, 2.57, 3.21, 3.86, 4.5, 5.14, 5.79, 6.43, 7.07, 7.71, 8.36, 9, 10, 11, 12 ]
            , expectNumbers "Us.toLinearScale" us.toLinearScale us.show (seqFromZero 20 us) [ -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ]
            , expectNumbers "Fr.toNum" Fr.toNum Fr.show (seqFromZero 20 fr) [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21 ]
            , expectNumbers "Br.toNum" Br.toNum Br.show (seqFromZero 20 br) [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21 ]
            ]
        , describe "Roundtrips" <|
            [ numericRoundtripExt "Fr.num" Fr.toNum Fr.fromNum fr.show (seqFromZero 20 fr)
            , numericRoundtripExt "Br.num" Br.toNum Br.fromNum br.show (seqFromZero 20 br)
            , numericRoundtrip "Font.linearScale" font () (seqFromZero 15 font)
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
