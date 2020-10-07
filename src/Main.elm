module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html.Styled as H
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Parser as P exposing ((|.), (|=), Parser)


type alias Model =
    String


type Msg
    = RawInputChanged String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = "< 1 , 2 ,3,4 >"
        , update = update
        , view = view >> H.toUnstyled
        }


type HappyResult
    = HappyFlatVector FloatVector
    | HappyRecursivePoint PointNode


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.textarea
            [ HA.value model
            , HE.onInput RawInputChanged
            ]
            []
        , H.text <| Debug.toString <| P.run happyParser model
        ]


update : Msg -> Model -> Model
update msg _ =
    case msg of
        RawInputChanged s ->
            s


happyParser : Parser HappyResult
happyParser =
    P.oneOf
        [ P.succeed HappyFlatVector
            |= floatVector
        , P.succeed HappyRecursivePoint
            |= point
        ]



-- Parsing
{-
   the eventual goal is to handle crummy output from go's map debug like
    map[key:value]
    map[count:2 users:[map[name:John Appleseed phone:1122555] map[name:Jane Jacobs phone:998855]]]
    we can rely on var-like keys, but the values can be anything.
    We can try to make them strings and go from there
-}


type alias Map a =
    Dict String a


type alias NearlyDict =
    List ( String, Node )


type Node
    = MapLeaf String
    | MapBranch NearlyDict
    | ArrayBranch (List Node)



-- Ugh, impossible to grok. Start with the given example and make it MORE
-- First let's do a recursive point that can handle things like
-- (   1,    (   1,    3  )     )


type alias PointBranch =
    { x : PointNode
    , y : PointNode
    }


type PointNode
    = Branch PointBranch
    | Leaf Float


branch : PointNode -> PointNode -> PointNode
branch x y =
    Branch <| PointBranch x y


point : Parser PointNode
point =
    P.succeed branch
        |. P.symbol "("
        |= insidePoint
        |. P.symbol ","
        |= insidePoint
        |. P.symbol ")"


thunkPoint : () -> Parser PointNode
thunkPoint () =
    point


insidePoint : Parser PointNode
insidePoint =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
            [ P.succeed Leaf
                |= P.float
            , P.succeed identity
                |= P.lazy thunkPoint
            ]
        |. P.spaces



{- That…wasn't terrible all things considered
   let's see if we can make it loop so it works on an array
   Well first we'll do just a 1D Vector
-}
-- FloatVector


type alias FloatVector =
    List Float


floatVector : Parser FloatVector
floatVector =
    P.succeed identity
        |. P.token "<"
        |. P.spaces
        |= P.loop [] floatVectorEnd


floatVectorEnd : FloatVector -> Parser (P.Step FloatVector FloatVector)
floatVectorEnd rev =
    P.oneOf
        [ P.succeed (appendAndLoop rev)
            |. P.spaces
            |. P.symbol ","
            |= parseSpacyFloat
        , P.succeed (appendAndLoop rev)
            |= parseSpacyFloat
        , P.succeed (P.Done (List.reverse rev))
            |. P.spaces
            |. P.token ">"
        ]


parseSpacyFloat : Parser Float
parseSpacyFloat =
    P.succeed identity
        |. P.spaces
        |= P.float
        |. P.spaces


appendAndLoop : FloatVector -> Float -> P.Step FloatVector a
appendAndLoop vec float =
    P.Loop (float :: vec)
