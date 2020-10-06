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
    = HappyFlatVector Vector
    | HappyRecursivePoint Inner


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
            |= vector
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
    = Leaf String
    | MapBranch NearlyDict
    | ArrayBranch (List Node)



-- Ugh, impossible to grok. Start with the given example and make it MORE
-- First let's do a recursive point that can handle things like
-- (   1,    (   1,    3  )     )


type alias Point =
    { x : Inner
    , y : Inner
    }


type Inner
    = Pointy Point
    | Floaty Float


pointyHelper : Inner -> Inner -> Inner
pointyHelper x y =
    Pointy <| Point x y


point : Parser Inner
point =
    P.succeed pointyHelper
        |. P.symbol "("
        |. P.spaces
        |= childParser
        |. P.spaces
        |. P.symbol ","
        |. P.spaces
        |= childParser
        |. P.spaces
        |. P.symbol ")"


childParser : Parser Inner
childParser =
    P.oneOf
        [ P.succeed Floaty
            |= P.float
        , P.succeed identity
            |= P.lazy (\_ -> point)
        ]



{- That…wasn't terrible all things considered
   let's see if we can make it loop so it works on an array
   Well first we'll do just a 1D Vector
-}
-- FlatVector


type alias Vector =
    List Float


vector : Parser Vector
vector =
    P.succeed identity
        |. P.token "<"
        |. P.spaces
        |= P.loop [] vectorEnd


vectorEnd : Vector -> Parser (P.Step Vector Vector)
vectorEnd rev =
    P.oneOf
        [ P.succeed (appendAndLoop rev)
            |. P.spaces
            |. P.symbol ","
            |. P.spaces
            |= P.float
            |. P.spaces
        , P.succeed (appendAndLoop rev)
            |. P.spaces
            |= P.float
            |. P.spaces
        , P.succeed (P.Done (List.reverse rev))
            |. P.spaces
            |. P.token ">"
        ]


appendAndLoop : Vector -> Float -> P.Step Vector a
appendAndLoop vec float =
    P.Loop (float :: vec)
