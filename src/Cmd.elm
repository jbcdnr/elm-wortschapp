module Cmd exposing (..)

import Model exposing (..)
import MyList as List
import List.Extra as List
import Random
import DeckShuffle exposing (..)
import Http


getDeck : String -> Cmd Msg
getDeck url =
    let
        request =
            Http.getString url
    in
        Http.send NewDeck request


getIndex : String -> Cmd Msg
getIndex url =
    let
        request =
            Http.getString url
    in
        Http.send NewIndex request


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

csvToSources : String -> List Source
csvToSources text =
    String.split "\n" text
        |> List.map (String.split ",")
        |> List.map uncons2
        |> List.flatten
        |> List.map
            (\( name, url, _ ) ->
                Source (String.trim name) (String.trim url)
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
