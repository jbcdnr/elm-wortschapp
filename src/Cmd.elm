module Cmd exposing (..)

import Model exposing (..)
import MyList as List
import List.Extra as List
import Random
import DeckShuffle exposing (..)
import Http


getDeck : Cmd Msg
getDeck =
    let
        url =
            "https://dl.dropboxusercontent.com/s/6h82np562bctp36/voc.csv?dl=0"

        request =
            Http.getString url
    in
        Http.send NewDeck request


csvToDeck : String -> Deck
csvToDeck text =
    String.split "\n" text
        |> List.map (String.split ",")
        |> List.map uncons2
        |> List.flatten
        |> List.map
            (\( front, back, tags ) ->
                Card (String.trim front) (String.trim back) (tags |> List.map String.trim |> List.filter (\x -> not <| String.isEmpty x))
            )


uncons2 : List a -> Maybe ( a, a, List a )
uncons2 ls =
    case List.uncons ls of
        Nothing ->
            Nothing

        Just ( first, rest ) ->
            case List.uncons rest of
                Nothing ->
                    Nothing

                Just ( second, rest ) ->
                    Just ( first, second, rest )
