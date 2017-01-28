module Main exposing (..)

import Html
import Http
import View exposing (..)
import Model exposing (..)
import Update exposing (..)
import Cmd exposing (..)


main =
    Html.program
        { init = ( defaultModel, getDeck "https://dl.dropboxusercontent.com/s/6h82np562bctp36/voc.csv?dl=0" )
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }
