module Grades.Generic exposing
    ( Generic, boulder, climb
    , br, font, fr, us, vgrade
    )

{-| Abstracts the Bouldering vs. Climbing grades.

The grading API is provided as attributes from specially constructed records.


## APIs

@docs Generic, boulder, climb


## Specific systems

@docs br, font, fr, us, vgrade

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
    { show = Us.show
    , showAs = \_ -> Us.show
    , parse = Us.parse
    , parseAs = \_ -> Us.parse
    , simplify = Us.simplify
    , withMod = Us.withMod
    , next = Us.next
    , prev = Us.prev
    , to = \_ x -> x
    , zero = Us.zero
    , compare = Us.order
    , toLinearScale = Us.toLinearScale
    , fromLinearScale = \_ -> Us.fromLinearScale
    }


{-| French grading system
-}
fr : Generic () Fr.Grade
fr =
    { show = Fr.show
    , showAs = \_ -> Fr.show
    , parse = Fr.parse
    , parseAs = \_ -> Fr.parse
    , simplify = Fr.simplify
    , withMod = Fr.withMod
    , next = Fr.next
    , prev = Fr.prev
    , to = \_ x -> x
    , zero = Fr.zero
    , compare = Fr.order
    , toLinearScale = Fr.toLinearScale
    , fromLinearScale = \_ -> Fr.fromLinearScale
    }


{-| Brazilian grading system
-}
br : Generic () Br.Grade
br =
    { show = Br.show
    , showAs = \_ -> Br.show
    , parse = Br.parse
    , parseAs = \_ -> Br.parse
    , simplify = Br.simplify
    , withMod = Br.withMod
    , next = Br.next
    , prev = Br.prev
    , to = \_ x -> x
    , zero = Br.zero
    , compare = Br.order
    , toLinearScale = Br.toLinearScale
    , fromLinearScale = \_ -> Br.fromLinearScale
    }


{-| Fontainebleau grading system
-}
font : Generic () Font.Grade
font =
    { show = Font.show
    , showAs = \_ -> Font.show
    , parse = Font.parse
    , parseAs = \_ -> Font.parse
    , simplify = Font.simplify
    , withMod = Font.withMod
    , next = Font.next
    , prev = Font.prev
    , to = \_ x -> x
    , zero = Font.zero
    , compare = Font.order
    , toLinearScale = Font.toLinearScale
    , fromLinearScale = \_ -> Font.fromLinearScale
    }


{-| Hueco V-grades system
-}
vgrade : Generic () Hueco.Grade
vgrade =
    { show = Hueco.show
    , showAs = \_ -> Hueco.show
    , parse = Hueco.parse
    , parseAs = \_ -> Hueco.parse
    , simplify = Hueco.simplify
    , withMod = Hueco.withMod
    , next = Hueco.next
    , prev = Hueco.prev
    , to = \_ x -> x
    , zero = Hueco.zero
    , compare = Hueco.order
    , toLinearScale = Hueco.toLinearScale
    , fromLinearScale = \_ -> Hueco.fromLinearScale
    }
