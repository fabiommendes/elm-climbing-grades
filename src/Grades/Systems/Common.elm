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
import Grades.Levels.Mod exposing (Mod, showModSoftHard)


select : Bool -> c -> c -> c
select cond a b =
    if cond then
        a

    else
        b


type alias FrGrade a =
    { a | mod : Mod, n : Int, cat : ABCPlus.Level }


type alias ShowFunc a =
    (a -> String) -> (a -> a) -> (a -> a) -> a -> String


{-| Show only number from french system, .e.g. "3"
-}
showFrNumber : ShowFunc (FrGrade a)
showFrNumber show simplify next g =
    case showModSoftHard g.mod of
        Just suffix ->
            String.fromInt g.n ++ suffix

        Nothing ->
            String.fromInt g.n ++ "/" ++ show (g |> simplify |> next)


{-| Show only number and optional + modifier from french system, .e.g. "4+"
-}
showFrPlus : ShowFunc (FrGrade a)
showFrPlus show simplify next g =
    case showModSoftHard g.mod of
        Just suffix ->
            String.fromInt g.n ++ select (ABCPlus.toABC g.cat == ABC.A) "" "+" ++ suffix

        Nothing ->
            case ABCPlus.showHalfway g.cat of
                Just suffix ->
                    String.fromInt g.n ++ suffix

                Nothing ->
                    show (g |> simplify) ++ "/" ++ show (g |> simplify |> next)


{-| Show only number and category (a, b, c) from french system, , .e.g. "6a"
-}
showFrCat : ShowFunc (FrGrade a)
showFrCat show simplify next g =
    let
        cat =
            ABCPlus.toABC g.cat
    in
    case showModSoftHard g.mod of
        Just suffix ->
            String.fromInt g.n ++ ABC.show cat ++ suffix

        Nothing ->
            case ABC.showHalfway cat of
                Just suffix ->
                    String.fromInt g.n ++ suffix

                Nothing ->
                    show (g |> simplify) ++ "/" ++ show (g |> simplify |> next)


{-| Show full grade from french system, , .e.g. "8b+"
-}
showFrFull : ShowFunc (FrGrade a)
showFrFull show simplify next g =
    case showModSoftHard g.mod of
        Just suffix ->
            String.fromInt g.n ++ ABCPlus.show g.cat ++ suffix

        Nothing ->
            case ABCPlus.showHalfway g.cat of
                Just suffix ->
                    String.fromInt g.n ++ suffix

                Nothing ->
                    show (g |> simplify) ++ "/" ++ show (g |> simplify |> next)
