module BinaryTree exposing (BinaryTree(..), PointBranch, parse)

import Parser as P exposing ((|.), (|=), Parser)


type alias PointBranch =
    { x : BinaryTree
    , y : BinaryTree
    }


type BinaryTree
    = Branch PointBranch
    | Leaf Float


branch : BinaryTree -> BinaryTree -> BinaryTree
branch x y =
    Branch <| PointBranch x y


parse : Parser BinaryTree
parse =
    P.succeed branch
        |. P.symbol "("
        |= insidePoint
        |. P.symbol ","
        |= insidePoint
        |. P.symbol ")"


thunkPoint : () -> Parser BinaryTree
thunkPoint () =
    parse


insidePoint : Parser BinaryTree
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
