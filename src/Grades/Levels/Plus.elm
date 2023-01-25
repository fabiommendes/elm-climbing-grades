module Grades.Levels.Plus exposing (Level(..), next, prev, show, showHalfWay)


type Level
    = Base
    | Plus


next : Level -> Maybe Level
next lvl =
    case lvl of
        Base ->
            Just Plus

        Plus ->
            Nothing


prev : Level -> Maybe Level
prev lvl =
    case lvl of
        Base ->
            Nothing

        Plus ->
            Just Base


show : Level -> String
show lvl =
    case lvl of
        Base ->
            ""

        Plus ->
            "+"


showHalfWay : Level -> Maybe String
showHalfWay lvl =
    case lvl of
        Base ->
            Just "/+"

        Plus ->
            Nothing
