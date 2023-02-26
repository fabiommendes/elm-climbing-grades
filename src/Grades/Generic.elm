module Grades.Generic exposing
    ( Generic, boulder, climb
    , br, font, fr, us, vgrade
    , yds
    )

{-| Abstracts the Bouldering vs. Climbing grades.

The grading API is provided as attributes from specially constructed records.


## APIs

@docs Generic, boulder, climb


## Specific systems

@docs br, font, fr, us, yds, vgrade

-}

import Grades.Bouldering as Boulder
import Grades.Climbing as Climbing
import Grades.Levels.Mod as Mod
import Grades.Systems.Br as Br
import Grades.Systems.Font as Font
import Grades.Systems.Fr as Fr
import Grades.Systems.Hueco as Hueco
import Grades.Systems.Us as Us


{-| Recording storing functions that implement the
API for handling climbing and bouldering grades
-}
type alias Generic sys grade =
    { show : grade -> String
    , showAs : sys -> grade -> String
    , parse : String -> Maybe grade
    , parseAs : sys -> String -> Maybe grade
    , simplify : grade -> grade
    , withMod : Mod.Mod -> grade -> grade
    , next : grade -> grade
    , prev : grade -> grade
    , to : sys -> grade -> grade
    , zero : grade
    , compare : grade -> grade -> Order
    , toLinearScale : grade -> Float
    , fromLinearScale : sys -> Float -> grade
    }


type alias GenericBase sys grade =
    { show : grade -> String
    , showAs : sys -> grade -> String
    , parse : String -> Maybe grade
    , parseAs : sys -> String -> Maybe grade
    , to : sys -> grade -> grade
    , zero : grade
    , toLinearScale : grade -> Float
    , fromLinearScale : sys -> Float -> grade
    }


generic : GenericBase sys Float -> Generic sys Float
generic { show, showAs, parse, parseAs, to, zero, toLinearScale, fromLinearScale } =
    { show = show
    , showAs = showAs
    , parse = parse
    , parseAs = parseAs
    , to = to
    , zero = zero
    , toLinearScale = toLinearScale
    , fromLinearScale = fromLinearScale
    , simplify = Mod.toBase
    , withMod = \mod -> Mod.toBase >> (+) (Mod.toLinearScale mod)
    , next = (+) 1
    , prev = (+) -1
    , compare = compare
    }


{-| Generic bouldering grades
-}
boulder : Generic Boulder.System Boulder.Grade
boulder =
    { show = Boulder.show
    , showAs = Boulder.showAs
    , parse = Boulder.parse
    , parseAs = Boulder.parseAs
    , simplify = Boulder.simplify
    , withMod = Boulder.withMod
    , next = Boulder.next
    , prev = Boulder.prev
    , to = Boulder.to
    , zero = Boulder.zero
    , compare = Boulder.compare
    , toLinearScale = Boulder.toLinearScale
    , fromLinearScale = Boulder.fromLinearScale
    }


{-| Generic climbing grades
-}
climb : Generic Climbing.System Climbing.Grade
climb =
    { show = Climbing.show
    , showAs = Climbing.showAs
    , parse = Climbing.parse
    , parseAs = Climbing.parseAs
    , simplify = Climbing.simplify
    , withMod = Climbing.withMod
    , next = Climbing.next
    , prev = Climbing.prev
    , to = Climbing.to
    , zero = Climbing.zero
    , compare = Climbing.compare
    , toLinearScale = Climbing.toLinearScale
    , fromLinearScale = Climbing.fromLinearScale
    }


{-| Yosemite decimal grading system
-}
us : Generic () Us.Grade
us =
    generic
        { show = Us.show
        , showAs = \_ -> Us.show
        , parse = Us.parse
        , parseAs = \_ -> Us.parse
        , to = \_ x -> x
        , zero = Us.zero
        , toLinearScale = identity
        , fromLinearScale = \_ -> identity
        }


{-| Yosemite decimal grading system. An alias to `us`
-}
yds : Generic () Us.Grade
yds =
    us


{-| French grading system
-}
fr : Generic () Fr.Grade
fr =
    generic
        { show = Fr.show
        , showAs = \_ -> Fr.show
        , parse = Fr.parse
        , parseAs = \_ -> Fr.parse
        , to = \_ x -> x
        , zero = Fr.zero
        , toLinearScale = Fr.toLinearScale
        , fromLinearScale = \_ -> Fr.fromLinearScale
        }


{-| Brazilian grading system
-}
br : Generic () Br.Grade
br =
    generic
        { show = Br.show
        , showAs = \_ -> Br.show
        , parse = Br.parse
        , parseAs = \_ -> Br.parse
        , to = \_ x -> x
        , zero = Br.zero
        , toLinearScale = Br.toLinearScale
        , fromLinearScale = \_ -> Br.fromLinearScale
        }


{-| Fontainebleau grading system
-}
font : Generic () Font.Grade
font =
    generic
        { show = Font.show
        , showAs = \_ -> Font.show
        , parse = Font.parse
        , parseAs = \_ -> Font.parse
        , to = \_ x -> x
        , zero = Font.zero
        , toLinearScale = Font.toLinearScale
        , fromLinearScale = \_ -> Font.fromLinearScale
        }


{-| Hueco V-grades system
-}
vgrade : Generic () Hueco.Grade
vgrade =
    generic
        { show = Hueco.show
        , showAs = \_ -> Hueco.show
        , parse = Hueco.parse
        , parseAs = \_ -> Hueco.parse
        , to = \_ x -> x
        , zero = Hueco.zero
        , toLinearScale = identity
        , fromLinearScale = \_ -> identity
        }
