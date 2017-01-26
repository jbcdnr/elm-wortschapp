module Main exposing (..)

import Html
import Http
import View exposing (..)
import Model exposing (..)
import Update exposing (..)


main =
    Html.program
        { init = ( defaultModel, getDeck )
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


defaultModel =
    Model
        []
        (Card "" "")
        False
        []
        Both


getDeck : Cmd Msg
getDeck =
    let
        url =
            "https://dl.dropboxusercontent.com/s/6h82np562bctp36/voc.csv?dl=0"

        request =
            Http.getString url
    in
        Http.send NewDeck request
