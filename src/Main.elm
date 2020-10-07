module Main exposing (main)

import BinaryTree
import Browser
import DeepVector
import Dict exposing (Dict)
import FloatVector
import Html.Styled as H
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Parser as P exposing ((|=), Parser)


type alias Model =
    String


type Msg
    = RawInputChanged String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = "[ 1 , 2 ,3,4 ]"
        , update = update
        , view = view >> H.toUnstyled
        }


type ParseResult
    = FloatVector FloatVector.FloatVector
    | BinaryTree BinaryTree.BinaryTree
    | DeepVector DeepVector.DeepVector


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.textarea
            [ HA.value model
            , HE.onInput RawInputChanged
            ]
            []
        , H.pre []
            [ H.text <| Debug.toString <| P.run happyParser model
            ]
        ]


update : Msg -> Model -> Model
update msg _ =
    case msg of
        RawInputChanged s ->
            s


happyParser : Parser ParseResult
happyParser =
    P.oneOf
        [ P.succeed FloatVector
            |= FloatVector.parse
        , P.succeed BinaryTree
            |= BinaryTree.parse
        , P.succeed DeepVector
            |= DeepVector.parse
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
-- see RecursivePoint.elm
{- That…wasn't terrible all things considered
   let's see if we can make it loop so it works on an array
   Well first we'll do just a 1D Vector
-}
-- moved contents to FloatVector.elm
