module Main exposing (..)

import Html
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
        AllWays
