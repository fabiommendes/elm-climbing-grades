module Grades.Parser exposing
    ( abc
    , abcHalfway
    , brParser
    , brParserHalfway
    , fontParser
    , frParser
    , fromResult
    , maybe
    , mod
    , optionalSuffix
    , plusModThen
    , roman
    , usParser
    , vParser
    , validateWith
    )

import Grades.Levels.ABC as ABC
import Grades.Levels.ABCD as ABCD
import Grades.Levels.ABCPlus as ABCPlus
import Grades.Levels.Mod as Mod exposing (..)
import Parser exposing (..)
import Roman exposing (fromRoman)



-------------------------------------------------------------------------------
--- Grade parsers
-------------------------------------------------------------------------------


vParser : (Int -> Mod -> a) -> Parser a
vParser fn =
    let
        validate n rmod =
            case rmod of
                Ok m ->
                    Ok <| fn n m

                Err m ->
                    if m == n + 1 then
                        Ok <| fn n Mod.HalfwayNext

                    else
                        Err "not a sequence of grades"
    in
    succeed validate
        |. token "V"
        |= oneOf [ succeed -1 |. symbol "b", int ]
        |= plusModThen int
        |. end
        |> fromResult


fontParser : (Int -> ABCPlus.Level -> Mod -> b) -> Parser b
fontParser fn =
    oneOf
        [ succeed fn
            |= int
            |= oneOf
                [ succeed ABCPlus.APlus |. token "a+"
                , succeed ABCPlus.A |. token "a"
                , succeed ABCPlus.BPlus |. token "b+"
                , succeed ABCPlus.B |. token "b"
                , succeed ABCPlus.CPlus |. token "c+"
                , succeed ABCPlus.C |. token "c"
                ]
            |. spaces
            |= mod
            |. end
        ]


brParser : (Int -> ABC.Level -> Mod -> b) -> Parser b
brParser fn =
    oneOf
        [ succeed (fn 6 ABC.B Mod.HalfwayNext) |. token "VIsup/7a"
        , succeed (fn 6 ABC.B Mod.HalfwayNext) |. token "6sup/7a"
        , backtrackable <|
            succeed fn
                |= roman
                |= oneOf
                    [ succeed ABC.B |. keyword "sup"
                    , succeed ABC.A
                    ]
                |. spaces
                |= mod
                |. end
        , backtrackable <|
            succeed fn
                |= int
                |= abc
                |. spaces
                |= mod
                |. end
        , brParserHalfway (\x y -> fn x y Mod.HalfwayNext)
        ]


brParserHalfway : (Int -> ABC.Level -> b) -> Parser b
brParserHalfway fn =
    oneOf
        [ succeed fn
            |= roman
            |= succeed ABC.A
            |. keyword "/sup"
            |. end
        , succeed fn
            |= int
            |= abcHalfway
            |. end
        ]


frParser : (Int -> ABCPlus.Level -> Mod -> b) -> Parser b
frParser fn =
    oneOf
        [ succeed fn
            |= int
            |= oneOf
                [ succeed ABCPlus.APlus |. token "a+"
                , succeed ABCPlus.A |. token "a"
                , succeed ABCPlus.BPlus |. token "b+"
                , succeed ABCPlus.B |. token "b"
                , succeed ABCPlus.CPlus |. token "c+"
                , succeed ABCPlus.C |. token "c"
                ]
            |. spaces
            |= mod
            |. end
        ]


usParser : (Int -> ABCD.Level -> Mod -> b) -> Parser b
usParser fn =
    let
        valid n ( hasLevel, lvl ) mod_ =
            if (n >= 10 && hasLevel) || (n < 10 && not hasLevel) then
                Ok (fn n lvl mod_)

            else
                Err "invalid range"
    in
    oneOf
        [ succeed valid
            |. token "5."
            |= int
            |= oneOf
                [ succeed ( True, ABCD.A ) |. token "a"
                , succeed ( True, ABCD.B ) |. token "b"
                , succeed ( True, ABCD.C ) |. token "c"
                , succeed ( True, ABCD.D ) |. token "d"
                , succeed ( False, ABCD.A )
                ]
            |. spaces
            |= mod
            |. end
            |> fromResult
        ]


validateWith : (a -> Result String b) -> Parser a -> Parser b
validateWith validator p =
    p
        |> andThen
            (\x ->
                case validator x of
                    Ok res ->
                        succeed res

                    Err st ->
                        problem st
            )



-------------------------------------------------------------------------------
--- Utility parsers
-------------------------------------------------------------------------------


roman : Parser Int
roman =
    (getChompedString <|
        chompWhile (\c -> String.contains (String.fromChar c) "IVXLDCM")
    )
        |> andThen
            (\st ->
                case fromRoman st of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem "not a roman numeral"
            )


optionalSuffix : String -> Parser Bool
optionalSuffix suffix =
    oneOf
        [ succeed True
            |. token suffix
            |. end
        , succeed False
            |. end
        ]


plusModThen : Parser a -> Parser (Result a Mod)
plusModThen p =
    oneOf
        [ succeed (Ok Hard)
            |. token "+"
        , succeed (Ok Soft)
            |. token "-"
        , succeed Err
            |. token "/"
            |= p
        , succeed (Ok Base)
        ]


mod : Parser Mod
mod =
    oneOf
        [ succeed Base
            |. end
        , succeed Hard
            |. keyword "hard"
            |. end
        , succeed Soft
            |. keyword "soft"
            |. end
        ]


abc : Parser ABC.Level
abc =
    oneOf
        [ succeed ABC.A |. token "a"
        , succeed ABC.B |. token "b"
        , succeed ABC.C |. token "c"
        ]


abcHalfway : Parser ABC.Level
abcHalfway =
    oneOf
        [ succeed ABC.A |. token "a/b"
        , succeed ABC.B |. token "b/c"
        ]


maybe : String -> Parser (Maybe a) -> Parser a
maybe err p =
    p
        |> andThen
            (\x ->
                x
                    |> Maybe.map succeed
                    |> Maybe.withDefault (problem err)
            )


fromResult : Parser (Result String a) -> Parser a
fromResult =
    andThen
        (\x ->
            case x of
                Ok v ->
                    succeed v

                Err e ->
                    problem e
        )
