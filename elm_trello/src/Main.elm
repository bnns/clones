module Main exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode exposing (field)
import List.Extra exposing (getAt, removeAt, splitAt, elemIndex)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import Utils.Event exposing (..)
import Utils.Random exposing (..)
import Utils.LocalStorage exposing (save)


type Msg
    = NoOp
    | LoadCards (List Card)
    | NewCard Card
    | InputText String
    | ToggleAddCardForm ColId
    | AddCardConfirm ColId
    | ToggleEditTitleForm ColId
    | EditTitleConfirm ColId
    | ToggleEditColorForm ColId
    | EditColorConfirm ColId
    | ToggleEditCardForm CardId
    | EditCardConfirm CardId
    | DragCard CardPosition
    | Highlight CardPosition
    | MoveCard


type CardPosition
    = CardPosition ColId Int


type CardId
    = CardId String


type ColId
    = ColId String


type EditState
    = EditingCard CardId
    | AddingCard ColId
    | EditingTitle ColId
    | EditingColor ColId
    | NotEditing


type DragState
    = Dragging CardPosition
    | Hovering CardPosition CardPosition
    | Inert


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
    , dragState : DragState
    }


initCardText : List String
initCardText =
    [ "pure functions"
    , "the elm architecture"
    , "algebraic data types"
    , "amazingly friendly compiler messages"
    , "elm-format"
    , "type-safety"
    , "first-class semantic versioning"
    , "(model, msg) monad"
    , "simple refactoring"
    , "(almost) zero runtime exceptions"
    ]


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


defaultCard : Card
defaultCard =
    Card (CardId "card0") "" (ColId "col0")


initModel : Model
initModel =
    { columns = initColumns
    , cards = []
    , editState = NotEditing
    , editText = ""
    , dragState = Inert
    }


clearEditState : Model -> Model
clearEditState model =
    { model | editText = "", editState = NotEditing }


toggleEdit : EditState -> EditState -> EditState
toggleEdit current next =
    if current == next && current /= NotEditing then
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


createCards : Cmd Msg
createCards =
    Random.generate LoadCards loadInitialCards


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        ToggleAddCardForm colId ->
            { model
                | editState = toggleEdit model.editState (AddingCard colId)
            }
                ! []

        ToggleEditTitleForm colId ->
            { model
                | editState = toggleEdit model.editState (EditingTitle colId)
                , editText =
                    List.Extra.find (\c -> c.id == colId) model.columns
                        |> Maybe.map .title
                        |> Maybe.withDefault ""
            }
                ! []

        ToggleEditColorForm colId ->
            { model
                | editState = toggleEdit model.editState (EditingColor colId)
                , editText =
                    List.Extra.find (\c -> c.id == colId) model.columns
                        |> Maybe.map .backgroundColor
                        |> Maybe.withDefault ""
            }
                ! []

        ToggleEditCardForm cardId ->
            { model
                | editState = toggleEdit model.editState (EditingCard cardId)
                , editText =
                    List.Extra.find (\c -> c.id == cardId) model.cards
                        |> Maybe.map .text
                        |> Maybe.withDefault ""
            }
                ! []

        InputText val ->
            { model | editText = val } ! []

        LoadCards cards ->
            let
                newModel =
                    { model | cards = cards }
            in
                newModel ! [ save <| encodeState newModel ]

        NewCard card ->
            let
                newModel =
                    { model | cards = [ card ] ++ model.cards }
            in
                newModel ! [ save <| encodeState newModel ]

        AddCardConfirm colId ->
            (clearEditState model) ! [ Random.generate NewCard (createCard colId model.editText) ]

        EditTitleConfirm colId ->
            let
                newModel =
                    model
                        |> clearEditState
                        |> \m ->
                            { m
                                | columns = updateInList (\c -> { c | title = model.editText }) colId model.columns
                            }
            in
                newModel ! [ save <| encodeState newModel ]

        EditColorConfirm colId ->
            let
                newModel =
                    model
                        |> clearEditState
                        |> \m ->
                            { m
                                | columns = updateInList (\c -> { c | backgroundColor = model.editText }) colId model.columns
                            }
            in
                newModel ! [ save <| encodeState newModel ]

        EditCardConfirm cardId ->
            let
                newModel =
                    model
                        |> clearEditState
                        |> \m ->
                            { m
                                | cards = updateInList (\c -> { c | text = model.editText }) cardId model.cards
                            }
            in
                newModel ! [ save <| encodeState newModel ]

        DragCard startPos ->
            case model.dragState of
                Inert ->
                    { model | dragState = Dragging startPos } ! []

                _ ->
                    model ! []

        Highlight endPos ->
            case model.dragState of
                Dragging startPos ->
                    if startPos /= endPos then
                        { model | dragState = Hovering startPos endPos } ! []
                    else
                        model ! []

                Hovering startPos _ ->
                    if startPos /= endPos then
                        { model | dragState = Hovering startPos endPos } ! []
                    else
                        model ! []

                _ ->
                    model ! []

        MoveCard ->
            let
                newCards =
                    swapCards model.dragState model.cards

                newModel =
                    { model | cards = newCards, dragState = Inert }
            in
                newModel ! [ save <| encodeState newModel ]


cardIdToString : CardId -> String
cardIdToString (CardId val) =
    val


colIdToString : ColId -> String
colIdToString (ColId val) =
    val


encodeCol : Column -> Encode.Value
encodeCol { id, backgroundColor, title } =
    Encode.object
        [ ( "id", id |> colIdToString |> Encode.string )
        , ( "title", title |> Encode.string )
        , ( "backgroundColor", backgroundColor |> Encode.string )
        ]


encodeCard : Card -> Encode.Value
encodeCard { id, text, belongsTo } =
    Encode.object
        [ ( "id", id |> cardIdToString |> Encode.string )
        , ( "text", text |> Encode.string )
        , ( "belongsTo", belongsTo |> colIdToString |> Encode.string )
        ]


encodeState : Model -> Encode.Value
encodeState { cards, columns } =
    Encode.object
        [ ( "cards", cards |> List.map encodeCard |> Encode.list )
        , ( "columns", columns |> List.map encodeCol |> Encode.list )
        ]


cardPlaceholder : DragState -> CardPosition -> Html Msg
cardPlaceholder dragState pos =
    let
        highlightMe =
            case dragState of
                Hovering _ endPos ->
                    endPos == pos

                _ ->
                    False
    in
        div
            [ class "card-placeholder"
            , classList [ ( "highlight", highlightMe ) ]
            , attribute "ondragover" "return false"
            , onDragOver <| Highlight pos
            ]
            [ div [ class "transparent" ] [ text "placeholder" ] ]


toRawIndex : Int -> ColId -> List Card -> Int
toRawIndex index colId cards =
    let
        filtered =
            cards |> List.filter (\c -> c.belongsTo == colId)

        filteredLength =
            List.length filtered

        card =
            getAt index filtered
                |> Maybe.withDefault defaultCard
    in
        if filteredLength == 0 || filteredLength <= index then
            List.length cards
        else
            elemIndex card cards
                |> Maybe.withDefault (List.length cards)


swapCards : DragState -> List Card -> List Card
swapCards dragState cards =
    case dragState of
        Hovering (CardPosition startCol start) (CardPosition endCol end) ->
            let
                rawStart =
                    toRawIndex start startCol cards

                rawEnd =
                    toRawIndex end endCol cards

                insertIndex =
                    if rawStart < rawEnd then
                        rawEnd - 1
                    else
                        rawEnd

                card =
                    getAt rawStart cards
                        |> Maybe.map (\c -> { c | belongsTo = endCol })
                        |> Maybe.withDefault defaultCard
            in
                removeAt rawStart cards
                    |> splitAt insertIndex
                    |> \( left, right ) -> left ++ [ card ] ++ right

        _ ->
            cards


createCard : ColId -> String -> Random.Generator Card
createCard colId cardText =
    Random.map (\id -> Card (CardId id) cardText colId) randomIdString


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


draggingMe : CardPosition -> DragState -> Bool
draggingMe myPos dragState =
    case dragState of
        Dragging pos ->
            myPos == pos

        Hovering pos _ ->
            myPos == pos

        _ ->
            False


renderCard : DragState -> EditState -> String -> Int -> Card -> Html Msg
renderCard dragState editState editText index card =
    let
        myPos =
            CardPosition card.belongsTo index

        displayCard =
            div
                [ class "card card-display"
                , onDragStart <| DragCard myPos
                , attribute "draggable" "true"
                , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '')"
                ]
                [ div [ class "card-text noselect" ] [ text card.text ]
                , div [ onClick <| ToggleEditCardForm card.id ]
                    [ span [ class "fas fa-pencil-alt btn" ] []
                    ]
                ]

        displayForm =
            div []
                [ textarea [ onInput InputText, value editText ] []
                , div [ class "card-edit-row" ]
                    [ button
                        [ class "save-btn btn"
                        , onClick <| EditCardConfirm card.id
                        ]
                        [ text "Save" ]
                    , button
                        [ class "cancel-btn btn"
                        , onClick <| ToggleEditCardForm card.id
                        ]
                        [ text "Cancel" ]
                    ]
                ]
    in
        div []
            [ viewIf (editState /= (EditingCard card.id) && not (draggingMe myPos dragState)) displayCard
            , viewIf (editState == (EditingCard card.id)) displayForm
            ]


renderTitle : EditState -> String -> Column -> Html Msg
renderTitle editState editText col =
    if editState == EditingTitle col.id then
        div [ class "change-col-title" ]
            [ input [ value editText, onInput InputText ] []
            , button
                [ class "btn save-btn"
                , onClick <| EditTitleConfirm col.id
                ]
                [ text "Save" ]
            , button
                [ class "btn cancel-btn"
                , onClick <| ToggleEditTitleForm col.id
                ]
                [ text "Cancel" ]
            ]
    else if editState == EditingColor col.id then
        div [ class "change-col-title" ]
            [ input [ type_ "color", value editText, onInput InputText ] []
            , button
                [ class "btn save-btn"
                , onClick <| EditColorConfirm col.id
                ]
                [ text "Save" ]
            , button
                [ class "btn cancel-btn"
                , onClick <| ToggleEditColorForm col.id
                ]
                [ text "Cancel" ]
            ]
    else
        div [ class "col-title" ]
            [ div [] [ text col.title ]
            , div
                [ onClick <| ToggleEditTitleForm col.id
                , class "btn"
                ]
                [ span [ class "fas fa-pencil-alt" ] [] ]
            , div
                [ onClick <| ToggleEditColorForm col.id
                , class "btn"
                ]
                [ span [ class "fas fa-eye" ] [] ]
            ]


renderColumn : DragState -> EditState -> String -> List Card -> Column -> Html Msg
renderColumn dragState editState editText cards ({ title, id, backgroundColor } as col) =
    let
        filtered =
            cards
                |> List.filter (\c -> c.belongsTo == id)

        -- kind of ugly, but we need a last placeholder when using interweave
        lastPlaceholder =
            cardPlaceholder dragState (CardPosition id (List.length filtered))

        placeholders =
            filtered
                |> List.indexedMap (\i _ -> cardPlaceholder dragState (CardPosition id i))
                |> flip (++) [ lastPlaceholder ]

        myCards =
            filtered
                |> List.indexedMap (renderCard dragState editState editText)
                |> List.Extra.interweave placeholders

        showAddCardBtn =
            div
                [ onClick <| ToggleAddCardForm id
                , class "btn text-white noselect"
                , style [ ( "margin-top", "1em" ) ]
                ]
                [ text "Add a card"
                ]
    in
        div
            [ class "column"
            , style [ ( "background-color", backgroundColor ) ]
            , onDrop MoveCard
            ]
            [ renderTitle editState editText col
            , div [ attribute "ondragover" "event.dataTransfer.dropEffect = 'move'; return false;" ] myCards
            , viewIf (editState == AddingCard id) (renderAddForm col editText)
            , viewIf (editState /= AddingCard id) showAddCardBtn
            ]


viewIf : Bool -> Html msg -> Html msg
viewIf show content =
    if show then
        content
    else
        div [] []


renderAddForm : Column -> String -> Html Msg
renderAddForm col editText =
    div [ class "add-form" ]
        [ textarea [ onInput <| InputText, value editText ] []
        , a
            [ class "btn save-btn"
            , onClick <| AddCardConfirm col.id
            ]
            [ text "Add" ]
        , a
            [ class "btn cancel-btn"
            , onClick <| ToggleAddCardForm col.id
            ]
            [ text "Cancel" ]
        ]


view : Model -> Html Msg
view { columns, cards, editState, editText, dragState } =
    let
        content =
            columns
                |> List.map (renderColumn dragState editState editText cards)
                |> div [ class "row-container" ]
    in
        section [ id "main" ]
            [ div [ class "header" ] [ text "Trello in Elm" ]
            , div [] [ content ]
            ]


cardDecoder : Decode.Decoder (List Card)
cardDecoder =
    let
        decoder =
            Decode.map3 Card
                (field "id" (Decode.map CardId Decode.string))
                (field "text" Decode.string)
                (field "belongsTo" (Decode.map ColId Decode.string))
    in
        Decode.list decoder


colDecoder : Decode.Decoder (List Column)
colDecoder =
    let
        decoder =
            Decode.map3 Column
                (field "id" (Decode.map ColId Decode.string))
                (field "backgroundColor" Decode.string)
                (field "title" Decode.string)
    in
        Decode.list decoder


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        savedCards =
            Decode.decodeValue (Decode.at [ "state", "cards" ] cardDecoder) flags
                |> Result.withDefault []

        savedCols =
            Decode.decodeValue (Decode.at [ "state", "columns" ] colDecoder) flags
                |> Result.withDefault initColumns

        initCmd =
            if List.length savedCards > 0 then
                Cmd.none
            else
                createCards
    in
        { initModel
            | cards = savedCards
            , columns = savedCols
        }
            ! [ initCmd ]


main : Program Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
