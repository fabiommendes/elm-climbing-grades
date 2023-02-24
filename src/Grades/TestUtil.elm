module Grades.TestUtil exposing (..)

import Grades.Generic as Generic
import Grades.Levels.Mod as Mod
import Grades.Util exposing (flip, iterate)


fullSeqFromZero : Int -> Generic.Generic sys grade -> List grade
fullSeqFromZero n tt =
    iterate tt.next tt.zero n
        |> List.concatMap (\g -> List.map (flip tt.withMod g) [ Mod.Soft, Mod.Base, Mod.Hard, Mod.HalfwayNext ])


seqFromZero : Int -> Generic.Generic sys grade -> List grade
seqFromZero n tt =
    iterate tt.next tt.zero n
