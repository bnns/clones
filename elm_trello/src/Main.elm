module Main exposing (main)

import Json.Decode as Decode
import List.Extra exposing (getAt)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Util


type Msg
    = NoOp
    | LoadCards (List Card)
    | NewCard Card
    | InputText String
    | ToggleAddCardForm ColId
    | AddCardConfirm ColId
    | ToggleEditTitleForm ColId
    | EditTitleConfirm ColId
    | ToggleEditCardForm CardId
    | EditCardConfirm CardId
    | MoveCard CardPosition CardPosition


type CardPosition
    = ColName CardId


type CardId
    = CardId String


type ColId
    = ColId String


type EditState
    = EditingCard CardId
    | AddingCard ColId
    | EditingTitle ColId
    | NotEditing


type alias Identifiable a b =
    { a | id : b }


type alias Card =
    { id : CardId
    , text : String
    , belongsTo : ColId
    }


type alias Column =
    { id : ColId
    , backgroundColor : String
    , title : String
    }


type alias Model =
    { columns : List Column
    , cards : List Card
    , editState : EditState
    , editText : String
    }


initCardText : List String
initCardText =
    [ "pure functions", "the elm architecture", "algebraic data types", "amazingly friendly compiler messages", "elm-format", "type-safety", "first-class semantic versioning", "(model, msg) monad", "simple refactoring", "(almost) zero runtime exceptions" ]


initColumns : List Column
initColumns =
    [ ( "#8e6e95", "icebox", "col1" )
    , ( "#39a59c", "backlog", "col2" )
    , ( "#344759", "current", "col3" )
    , ( "#e8741e", "completed", "col4" )
    ]
        |> List.map (\( color, title, id ) -> Column (ColId id) color title)


defaultCol : Column
defaultCol =
    Column (ColId "col0") "#fff" "default"


initModel : Model
initModel =
    { columns = initColumns
    , cards = []
    , editState = NotEditing
    , editText = ""
    }


clearForm : Model -> Model
clearForm model =
    { model | editText = "" }


toggleEdit : EditState -> EditState -> EditState
toggleEdit current next =
    if current == next then
        NotEditing
    else
        next


numInitialCards : Int
numInitialCards =
    10


randomText : Random.Generator String
randomText =
    let
        getTextAt i =
            initCardText |> getAt (i - 1) |> Maybe.withDefault ""
    in
        Random.map getTextAt (Random.int 1 (List.length initCardText))


randomCard : String -> Random.Generator Card
randomCard text =
    let
        getCol i =
            initColumns |> getAt (i - 1) |> Maybe.withDefault defaultCol
    in
        Random.map getCol (Random.int 1 (List.length initColumns))
            |> Random.andThen (\col -> createCard col.id text)


loadInitialCards : Random.Generator (List Card)
loadInitialCards =
    randomText
        |> Random.andThen randomCard
        |> Random.list 10


createOrLoadCards : Cmd Msg
createOrLoadCards =
    Random.generate LoadCards loadInitialCards


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        ToggleAddCardForm colId ->
            { model | editState = toggleEdit model.editState (AddingCard colId) } ! []

        ToggleEditTitleForm colId ->
            { model | editState = toggleEdit model.editState (EditingTitle colId) } ! []

        ToggleEditCardForm cardId ->
            { model | editState = toggleEdit model.editState (EditingCard cardId) } ! []

        InputText val ->
            { model | editText = val } ! []

        LoadCards cards ->
            { model | cards = cards } ! []

        NewCard card ->
            { model | cards = [ card ] ++ model.cards } ! []

        AddCardConfirm colId ->
            { model | editText = "" } ! [ Random.generate NewCard (createCard colId model.editText) ]

        EditTitleConfirm colId ->
            { model
                | columns = updateInList (\c -> { c | title = model.editText }) colId model.columns
                , editText = ""
            }
                ! []

        EditCardConfirm cardId ->
            { model
                | cards = updateInList (\c -> { c | text = model.editText }) cardId model.cards
                , editText = ""
            }
                ! []

        MoveCard start end ->
            model ! []


createCard : ColId -> String -> Random.Generator Card
createCard colId cardText =
    Random.map (\id -> Card (CardId id) cardText colId) Util.randomIdString


updateInList : (Identifiable a b -> Identifiable a b) -> b -> List (Identifiable a b) -> List (Identifiable a b)
updateInList transform id list =
    list
        |> List.map
            (\i ->
                if i.id == id then
                    transform i
                else
                    i
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


renderCard : EditState -> String -> Card -> Html Msg
renderCard editState editText card =
    div [ class "card" ]
        [ div [] [ text card.text ]
        ]


renderColumn : EditState -> String -> List Card -> Column -> Html Msg
renderColumn editState editText cards { title, id, backgroundColor } =
    let
        myCards =
            cards
                |> List.filter (\c -> c.belongsTo == id)
                |> List.map (renderCard editState editText)
    in
        div
            [ class "column"
            , style [ ( "background-color", backgroundColor ) ]
            ]
            [ div
                [ class "col-title"
                ]
                [ span [ onClick <| ToggleEditTitleForm id, class "fas fa-pencil-alt btn" ] []
                , div [] [ text title ]
                ]
            , div [] myCards
            , viewIf (editState /= AddingCard id) (div [ onClick <| ToggleAddCardForm id, class "btn text-white" ] [ text "Add a card" ])
            ]


viewIf : Bool -> Html msg -> Html msg
viewIf show content =
    if show then
        content
    else
        div [] []


renderAddForm : Maybe ColId -> ColId -> String -> List (Html Msg) -> Html Msg
renderAddForm showFormIn colId formText colHtml =
    let
        form =
            div [ class "add-form" ]
                [ textarea [ onInput <| InputText, value formText ] []
                , a [ class "btn save-btn", onClick <| AddCardConfirm colId ] [ text "Add" ]
                , a [ class "btn cancel-btn", onClick <| ToggleAddCardForm colId ] [ text "Cancel" ]
                ]
    in
        if showFormIn == Just colId then
            div [] (colHtml ++ [ form ])
        else
            div [] colHtml


view : Model -> Html Msg
view { columns, cards, editState, editText } =
    let
        content =
            columns
                |> List.map (renderColumn editState editText cards)
                |> div [ class "row-container" ]
    in
        section [ id "main" ]
            [ div [ class "header" ] [ text "Trello in Elm" ]
            , div [] [ content ]
            ]


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    initModel ! [ createOrLoadCards ]


main : Program Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
