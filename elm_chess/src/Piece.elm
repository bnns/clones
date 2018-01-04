module Piece exposing (..)

import Square exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style, classList)


type Piece
    = Rook
    | Knight
    | Bishop
    | Queen
    | King
    | Pawn


type Color
    = White
    | Black


type alias MaxSteps =
    Maybe Int


type BoardPiece
    = BoardPiece Piece Color Square


occupied : Color -> Maybe Square -> List BoardPiece -> Bool
occupied color square pieces =
    pieces |> List.any (\(BoardPiece _ c sq) -> (Just sq) == square && c == color)


moves : List BoardPiece -> Color -> Maybe Square -> (Square -> Maybe Square) -> MaxSteps -> List Square -> List Square
moves pieces myColor mySquare dir maxSteps myMoves =
    let
        oppColor =
            if myColor == White then
                Black
            else
                White

        nextSquare =
            applyDirection mySquare dir

        nextValidSquare =
            if mySquare == Nothing || maxSteps == Just 0 || occupied oppColor mySquare pieces || occupied myColor nextSquare pieces then
                Nothing
            else
                nextSquare

        updatedMaxSteps =
            maxSteps |> Maybe.map (flip (-) 1)
    in
        case nextSquare of
            Just square ->
                moves pieces myColor (Just square) dir updatedMaxSteps (square :: myMoves)

            Nothing ->
                myMoves


rookMoves : Color -> Square -> List BoardPiece -> List Square
rookMoves color myPos pieces =
    (moves pieces color (Just myPos) left Nothing [])
        ++ (moves pieces color (Just myPos) right Nothing [])
        ++ (moves pieces color (Just myPos) up Nothing [])
        ++ (moves pieces color (Just myPos) down Nothing [])


knightMoves : Color -> Square -> List BoardPiece -> List Square
knightMoves color myPos pieces =
    (moves pieces color (Just myPos) left (Just 1) [])
        ++ (moves pieces color (Just myPos) right (Just 1) [])
        ++ (moves pieces color (Just myPos) up (Just 1) [])
        ++ (moves pieces color (Just myPos) down (Just 1) [])


pawnMoves : Color -> Square -> List BoardPiece -> List Square
pawnMoves color myPos pieces =
    (moves pieces color (Just myPos) up (Just 1) [])


bishopMoves : Color -> Square -> List BoardPiece -> List Square
bishopMoves color myPos pieces =
    (moves pieces color (Just myPos) left Nothing [])
        ++ (moves pieces color (Just myPos) right Nothing [])


queenMoves : Color -> Square -> List BoardPiece -> List Square
queenMoves color myPos pieces =
    (moves pieces color (Just myPos) left Nothing [])
        ++ (moves pieces color (Just myPos) right Nothing [])
        ++ (moves pieces color (Just myPos) up Nothing [])
        ++ (moves pieces color (Just myPos) down Nothing [])


kingMoves : Color -> Square -> List BoardPiece -> List Square
kingMoves color myPos pieces =
    (moves pieces color (Just myPos) left (Just 1) [])
        ++ (moves pieces color (Just myPos) right (Just 1) [])
        ++ (moves pieces color (Just myPos) up (Just 1) [])
        ++ (moves pieces color (Just myPos) down (Just 1) [])


validMoves : BoardPiece -> List BoardPiece -> List Square
validMoves piece allPieces =
    case piece of
        BoardPiece Rook color square ->
            rookMoves color square allPieces

        BoardPiece Knight color square ->
            knightMoves color square allPieces

        BoardPiece Bishop color square ->
            bishopMoves color square allPieces

        BoardPiece Queen color square ->
            queenMoves color square allPieces

        BoardPiece King color square ->
            kingMoves color square allPieces

        BoardPiece Pawn color square ->
            pawnMoves color square allPieces


viewPiece : BoardPiece -> Html msg
viewPiece piece =
    let
        ( pName, pStyle ) =
            case piece of
                BoardPiece Rook c (Square l n) ->
                    ( (colorToString c) ++ "R", pieceStyle l n )

                BoardPiece Knight c (Square l n) ->
                    ( (colorToString c) ++ "N", pieceStyle l n )

                BoardPiece Bishop c (Square l n) ->
                    ( (colorToString c) ++ "B", pieceStyle l n )

                BoardPiece Pawn c (Square l n) ->
                    ( (colorToString c) ++ "P", pieceStyle l n )

                BoardPiece King c (Square l n) ->
                    ( (colorToString c) ++ "K", pieceStyle l n )

                BoardPiece Queen c (Square l n) ->
                    ( (colorToString c) ++ "Q", pieceStyle l n )

        classes =
            [ ( pName, True )
            , ( "piece", True )
            ]
    in
        div [ classList classes, style pStyle ] [ div [] [] ]


pieceStyle : Letter -> Number -> List ( String, String )
pieceStyle letter number =
    let
        x =
            letter |> letterToOffset |> toString

        y =
            (8 - number) |> (*) squareSize |> toString
    in
        [ ( "transform", "translate(" ++ x ++ "px," ++ y ++ "px)" ) ]


colorToString : Color -> String
colorToString color =
    case color of
        White ->
            "w"

        Black ->
            "b"


bPawns : List BoardPiece
bPawns =
    [ BoardPiece Pawn Black (Square A 7)
    , BoardPiece Pawn Black (Square B 7)
    , BoardPiece Pawn Black (Square C 7)
    , BoardPiece Pawn Black (Square D 7)
    , BoardPiece Pawn Black (Square E 7)
    , BoardPiece Pawn Black (Square F 7)
    , BoardPiece Pawn Black (Square G 7)
    , BoardPiece Pawn Black (Square H 7)
    ]


wPawns : List BoardPiece
wPawns =
    [ BoardPiece Pawn White (Square A 2)
    , BoardPiece Pawn White (Square B 2)
    , BoardPiece Pawn White (Square C 2)
    , BoardPiece Pawn White (Square D 2)
    , BoardPiece Pawn White (Square E 2)
    , BoardPiece Pawn White (Square F 2)
    , BoardPiece Pawn White (Square G 2)
    , BoardPiece Pawn White (Square H 2)
    ]


startingPieces : List BoardPiece
startingPieces =
    [ BoardPiece Rook White (Square A 1)
    , BoardPiece Bishop White (Square B 1)
    , BoardPiece Knight White (Square C 1)
    , BoardPiece Queen White (Square D 1)
    , BoardPiece King White (Square E 1)
    , BoardPiece Knight White (Square F 1)
    , BoardPiece Bishop White (Square G 1)
    , BoardPiece Rook White (Square H 1)
    , BoardPiece Rook Black (Square A 8)
    , BoardPiece Bishop Black (Square B 8)
    , BoardPiece Knight Black (Square C 8)
    , BoardPiece Queen Black (Square D 8)
    , BoardPiece King Black (Square E 8)
    , BoardPiece Knight Black (Square F 8)
    , BoardPiece Bishop Black (Square G 8)
    , BoardPiece Rook Black (Square H 8)
    ]
        ++ wPawns
        ++ bPawns
