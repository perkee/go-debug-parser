module DeepVector exposing (DeepVector, parse)

import Parser as P exposing ((|.), (|=), Parser, float)


type alias DeepVector =
    List Node


type Node
    = Branch DeepVector
    | Leaf Float


thunkVector : () -> Parser DeepVector
thunkVector () =
    parse


nodeParser : Parser Node
nodeParser =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
            [ P.succeed Leaf
                |= P.float
            , P.succeed Branch
                |= P.lazy thunkVector
            ]


pZarse : Parser DeepVector
pZarse =
    P.succeed identity
        |. P.token "["
        |. P.spaces
        |= P.oneOf
            [ P.succeed []
                |. P.token "]"
            , P.succeed (::)
                |= nodeParser
                |. P.spaces
                |= P.loop [] floatVectorEnd
            ]


floatVectorEnd : DeepVector -> Parser (P.Step DeepVector DeepVector)
floatVectorEnd rev =
    P.oneOf
        [ P.succeed (appendAndLoop rev)
            |. P.symbol ","
            |. P.spaces
            |= nodeParser
            |. P.spaces
        , P.succeed (P.Done (List.reverse rev))
            |. P.spaces
            |. P.token "]"
        ]


appendAndLoop : DeepVector -> Node -> P.Step DeepVector a
appendAndLoop vec float =
    P.Loop (float :: vec)



-- Equivalent using the amazing helper fn.


parse : Parser DeepVector
parse =
    P.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = P.spaces
        , item = nodeParser
        , trailing = P.Forbidden
        }
