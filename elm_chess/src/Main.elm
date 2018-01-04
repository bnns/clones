module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (style, class, classList, src)
import Square exposing (..)
import Piece exposing (..)


---- MODEL ----


type alias Orientation =
    Color


type alias Model =
    { pieces : List BoardPiece, orientation : Orientation }


init : ( Model, Cmd Msg )
init =
    ( { pieces = startingPieces, orientation = White }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        pieces =
            model.pieces |> List.map viewPiece
    in
        div [ class "main" ]
            [ h1 [] [ text "Chess in Elm" ]
            , div [ class "board-wrap" ]
                [ div [ class "board" ] pieces
                ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
