module Main exposing (..)

import Html
import Http
import View exposing (..)
import Model exposing (..)
import Update exposing (..)
import Cmd exposing (..)


main =
    Html.program
        { init = ( defaultModel, getUrl "https://dl.dropboxusercontent.com/s/9vyxea7cntyc4d5/index.csv?dl=0" NewIndex )
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }
