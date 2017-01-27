module Main exposing (..)

import Html
import Http
import View exposing (..)
import Model exposing (..)
import Update exposing (..)
import Cmd exposing (..)


main =
    Html.program
        { init = ( defaultModel, getDeck )
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }
