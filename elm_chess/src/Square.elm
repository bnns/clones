module Square exposing (..)


squareSize : Int
squareSize =
    64


type Square
    = Square Letter Number


type Letter
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type alias Number =
    Int


letterToInt : Letter -> Int
letterToInt letter =
    case letter of
        A ->
            0

        B ->
            1

        C ->
            2

        D ->
            3

        E ->
            4

        F ->
            5

        G ->
            6

        H ->
            7


intToLetter : Int -> Letter
intToLetter val =
    case val of
        0 ->
            A

        1 ->
            B

        2 ->
            C

        3 ->
            D

        4 ->
            E

        5 ->
            F

        6 ->
            G

        _ ->
            H


letterToOffset : Letter -> Int
letterToOffset letter =
    let
        multiplier =
            letterToInt letter
    in
        multiplier * squareSize


applyDirection : Maybe Square -> (Square -> Maybe Square) -> Maybe Square
applyDirection start direction =
    start
        |> Maybe.map direction
        |> Maybe.withDefault start


up : Square -> Maybe Square
up (Square letter number) =
    if number < 7 then
        Just (Square letter (number + 1))
    else
        Nothing


down : Square -> Maybe Square
down (Square letter number) =
    if number > 1 then
        Just (Square letter (number - 1))
    else
        Nothing


left : Square -> Maybe Square
left (Square letter number) =
    let
        numLetter =
            letterToInt letter
    in
        if numLetter > 0 then
            Just (Square (intToLetter (numLetter - 1)) number)
        else
            Nothing


right : Square -> Maybe Square
right (Square letter number) =
    let
        numLetter =
            letterToInt letter
    in
        if numLetter < 7 then
            Just (Square (intToLetter (numLetter + 1)) number)
        else
            Nothing
