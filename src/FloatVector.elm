module FloatVector exposing (FloatVector, parse)

import Parser as P exposing ((|.), (|=), Parser, float)


type alias FloatVector =
    List Float


parse : Parser FloatVector
parse =
    P.succeed identity
        |. P.token "<"
        |. P.spaces
        |= P.oneOf
            [ P.succeed []
                |. P.token ">"
            , P.succeed (::)
                --(\first rest -> first :: rest)
                |= P.float
                |. P.spaces
                |= P.loop [] floatVectorEnd
            ]


floatVectorEnd : FloatVector -> Parser (P.Step FloatVector FloatVector)
floatVectorEnd rev =
    P.oneOf
        [ P.succeed (appendAndLoop rev)
            |. P.symbol ","
            |. P.spaces
            |= P.float
            |. P.spaces
        , P.succeed (P.Done (List.reverse rev))
            |. P.spaces
            |. P.token ">"
        ]


appendAndLoop : FloatVector -> Float -> P.Step FloatVector a
appendAndLoop vec float =
    P.Loop (float :: vec)



-- Equivalent using the amazing helper fn.


pZarse : Parser FloatVector
pZarse =
    P.sequence
        { start = "<"
        , separator = ","
        , end = ">"
        , spaces = P.spaces
        , item = P.float
        , trailing = P.Forbidden
        }
