port module Utils.LocalStorage exposing (save)

import Json.Decode exposing (Value)


port save : Value -> Cmd msg
