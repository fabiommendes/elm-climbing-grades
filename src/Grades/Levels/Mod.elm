module Grades.Levels.Mod exposing (Mod(..), fromLinearScale, order, show, showExt, showModSoftHard, toLinearScale)

{-| Sub-levels and categories inside a given grade class.
-}


{-| Difficulty modifier
-}
type Mod
    = Base
    | Soft
    | Hard
    | HalfwayNext


order : Mod -> Mod -> Order
order a b =
    compare (toLinearScale a) (toLinearScale b)


showExt : ( String, String ) -> Mod -> Maybe String
showExt mods mod =
    case mod of
        Base ->
            Just ""

        Soft ->
            Just <| Tuple.first mods

        Hard ->
            Just <| Tuple.second mods

        HalfwayNext ->
            Nothing


show : Mod -> Maybe String
show =
    showExt ( "-", "+" )


showModSoftHard : Mod -> Maybe String
showModSoftHard =
    showExt ( " soft", " hard" )


toLinearScale : Mod -> Float
toLinearScale mod =
    case mod of
        Soft ->
            -0.25

        Base ->
            0.0

        Hard ->
            0.25

        HalfwayNext ->
            0.5


fromLinearScale : number -> Float -> ( number, Mod )
fromLinearScale n x =
    if x < 0 then
        fromLinearScale (n - 1) (x + 1)

    else if x > 1 then
        fromLinearScale (n + 1) (x - 1)

    else if x < 0.125 then
        ( n, Base )

    else if x < 0.375 then
        ( n, Hard )

    else if x < 0.625 then
        ( n, HalfwayNext )

    else if x < 0.875 then
        ( n + 1, Soft )

    else
        ( n + 1, Base )
