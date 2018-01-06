module Main exposing (main)

import Json.Decode as Decode
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type Msg
    = NoOp
    | ShowForm ColName
    | InputCard String
    | AddCard ColName String
    | HideForm


type ColName
    = First
    | Second
    | Third
    | Fourth


type alias Card =
    String


type alias Column =
    { headerColor : String
    , cards : List Card
    }


type alias Model =
    { firstCol : Column
    , secondCol : Column
    , thirdCol : Column
    , fourthCol : Column
    , showForm : Maybe ColName
    , form : String
    }


initModel : Model
initModel =
    { firstCol = Column "#8e6e95" [ "Card 1", "Card 2" ]
    , secondCol = Column "#39a59c" [ "Card 3", "Card 4" ]
    , thirdCol = Column "#344759" []
    , fourthCol = Column "#e8741e" [ "Card 5" ]
    , showForm = Nothing
    , form = ""
    }


updateCardsInCol : Column -> Card -> Column
updateCardsInCol col card =
    { col | cards = col.cards ++ [ card ] }


updateCol : ColName -> Card -> Model -> Model
updateCol colName card model =
    case colName of
        First ->
            { model | firstCol = updateCardsInCol model.firstCol card }

        Second ->
            { model | secondCol = updateCardsInCol model.secondCol card }

        Third ->
            { model | thirdCol = updateCardsInCol model.thirdCol card }

        Fourth ->
            { model | fourthCol = updateCardsInCol model.fourthCol card }


clearForm : Model -> Model
clearForm model =
    { model | form = "" }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        AddCard colName val ->
            (updateCol colName val model |> clearForm) ! []

        ShowForm colName ->
            { model | showForm = Just colName } ! []

        HideForm ->
            { model | showForm = Nothing } ! []

        InputCard val ->
            { model | form = val } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


renderCard : Card -> Html Msg
renderCard card =
    div [ class "card" ]
        [ div [] [ text card ]
        ]


renderColumn : Bool -> Msg -> String -> String -> Html Msg -> Html Msg
renderColumn showForm addCard name color content =
    div
        [ class "column"
        , style [ ( "background-color", color ) ]
        ]
        [ h3
            [ class "col-title"
            ]
            [ text name ]
        , div [] [ content ]
        , viewIf (not showForm) (div [ onClick addCard, class "btn text-white" ] [ text "Add a card" ])
        ]


viewIf : Bool -> Html msg -> Html msg
viewIf show content =
    if show then
        content
    else
        div [] []


renderAddForm : Maybe ColName -> ColName -> String -> List (Html Msg) -> Html Msg
renderAddForm showFormIn colName cardVal colHtml =
    let
        form =
            div [ class "add-form" ]
                [ textarea [ onInput <| InputCard, value cardVal ] []
                , a [ class "btn save-btn", onClick <| AddCard colName cardVal ] [ text "Add" ]
                , a [ class "btn cancel-btn", onClick <| HideForm ] [ text "Cancel" ]
                ]
    in
        if showFormIn == Just colName then
            div [] (colHtml ++ [ form ])
        else
            div [] colHtml


view : Model -> Html Msg
view model =
    let
        firstColView =
            model.firstCol.cards
                |> List.map renderCard
                |> renderAddForm model.showForm First model.form
                |> renderColumn (model.showForm == Just First) (ShowForm First) "icebox" model.firstCol.headerColor

        secondColView =
            model.secondCol.cards
                |> List.map renderCard
                |> renderAddForm model.showForm Second model.form
                |> renderColumn (model.showForm == Just Second) (ShowForm Second) "backlog" model.secondCol.headerColor

        thirdColView =
            model.thirdCol.cards
                |> List.map renderCard
                |> renderAddForm model.showForm Third model.form
                |> renderColumn (model.showForm == Just Third) (ShowForm Third) "current" model.thirdCol.headerColor

        fourthColView =
            model.fourthCol.cards
                |> List.map renderCard
                |> renderAddForm model.showForm Fourth model.form
                |> renderColumn (model.showForm == Just Fourth) (ShowForm Fourth) "completed" model.fourthCol.headerColor

        content =
            div [ class "row-container" ]
                [ firstColView
                , secondColView
                , thirdColView
                , fourthColView
                ]
    in
        section [ id "main" ]
            [ div [ class "header" ] [ text "Trello in Elm" ]
            , div [] [ content ]
            ]


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    initModel ! []


main : Program Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
