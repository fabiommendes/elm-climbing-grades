module Grades.Systems.Common exposing
    ( FrGrade
    , ShowFunc
    , select
    , showFrCat
    , showFrFull
    , showFrNumber
    , showFrPlus
    )

import Grades.Levels.ABC as ABC
import Grades.Levels.ABCPlus as ABCPlus
import Grades.Levels.Mod exposing (Mod, showModSoftHard, toBase)


select : Bool -> c -> c -> c
select cond a b =
    if cond then
        a

    else
        b


type alias FrGrade =
    { n : Int, cat : ABCPlus.Level, mod : Mod }


type alias ShowFunc =
    (Float -> String) -> (Float -> FrGrade) -> Float -> String


{-| Show only number from french system, .e.g. "3"
-}
showFrNumber : ShowFunc
showFrNumber show split g =
    let
        { n, mod } =
            split g
    in
    case showModSoftHard mod of
        Just suffix ->
            String.fromInt n ++ suffix

        Nothing ->
            String.fromInt n ++ "/" ++ show (g |> toBase |> next)


{-| Show only number and optional + modifier from french system, .e.g. "4+"
-}
showFrPlus : ShowFunc
showFrPlus show split g =
    let
        { n, cat, mod } =
            split g
    in
    case showModSoftHard mod of
        Just suffix ->
            String.fromInt n ++ select (ABCPlus.toABC cat == ABC.A) "" "+" ++ suffix

        Nothing ->
            case ABCPlus.showHalfway cat of
                Just suffix ->
                    String.fromInt n ++ suffix

                Nothing ->
                    show (g |> toBase) ++ "/" ++ show (g |> toBase |> next)


{-| Show only number and category (a, b, c) from french system, , .e.g. "6a"
-}
showFrCat : ShowFunc
showFrCat show split g =
    let
        { n, cat, mod } =
            split g

        cat_ =
            ABCPlus.toABC cat
    in
    case showModSoftHard mod of
        Just suffix ->
            String.fromInt n ++ ABC.show cat_ ++ suffix

        Nothing ->
            case ABC.showHalfway cat_ of
                Just suffix ->
                    String.fromInt n ++ suffix

                Nothing ->
                    show (g |> toBase) ++ "/" ++ show (g |> toBase |> next)


{-| Show full grade from french system, , .e.g. "8b+"
-}
showFrFull : ShowFunc
showFrFull show split g =
    let
        { n, cat, mod } =
            split g
    in
    case showModSoftHard mod of
        Just suffix ->
            String.fromInt n ++ ABCPlus.show cat ++ suffix

        Nothing ->
            case ABCPlus.showHalfway cat of
                Just suffix ->
                    String.fromInt n ++ suffix

                Nothing ->
                    show (g |> toBase) ++ "/" ++ show (g |> toBase |> next)


next : number -> number
next =
    (+) 1
