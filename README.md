# Climbing grades in Elm

`elm-climbing-grades` implements parsers, converters and data structures to store climbing grades in Elm.

Currently, it supports the V-scale (Hueco scale) and Fontainebleau for Bouldering and the 
Yosemite Decimal Scale (5.x), French, and Brazilian systems for grading routes.  


# Examples

`elm-climbing-grades` expose two modules with very similar interfaces: `Grades.Bouldering` and 
`Grades.Climbing` that represent grades in their respective disciplines.

```elm

import Grades.Bouldering as Boulder


fontGrade = 
    Boulder.parse "V10"              -- parse a grade in the V-scale 
        |> Maybe.withDefault Boulder.zero
        |> to Boulder.Fontainebleau   -- convert to Fontainebleau
        |> Boulder.simplify           -- remove any eventual modifier
        |> Boulder.next               -- bump one difficulty level
        |> show font                  -- renders the Fontainebleau grade
```

Bouldering grading systems

* VGrade, the Hueco/Vermin v-scale used in bouldering. Ex: VB, V0, V11
* Fontainebleau: Fontainebleau scale. Ex: 3, 5a, 7c+

Lead climbing grading systems

* US: American scale for routes, the Yosemite Decimal System. Ex: 3, 5.9, 5.12d 
* FR: French scale for routes. Similar to Fontainebleau, but not identical. Ex: 4, 5+, 6c, 8a+
* BR: Brazilian scale for routes. Somewhat inspired by the French system. Ex: IV, VIsup, 9c