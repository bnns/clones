module Utils.Event
    exposing
        ( onClickNoBubble
        , onDragStart
        , onDragOver
        , onDrop
        )

import Html exposing (Attribute)
import Html.Events exposing (on, keyCode, onWithOptions, defaultOptions)
import Json.Decode as Decode


onDragStart : msg -> Attribute msg
onDragStart message =
    on "dragstart" (Decode.succeed message)


onDrop : msg -> Attribute msg
onDrop message =
    onPreventHelper "drop" message


onDragOver : msg -> Attribute msg
onDragOver message =
    onPreventHelper "dragover" message


onPreventHelper : String -> msg -> Attribute msg
onPreventHelper eventName message =
    onWithOptions
        eventName
        { preventDefault = True
        , stopPropagation = False
        }
        (Decode.succeed message)


onClickNoBubble : msg -> Attribute msg
onClickNoBubble message =
    onWithOptions "click"
        { defaultOptions | stopPropagation = True }
        (Decode.succeed message)
