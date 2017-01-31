module Cmd exposing (..)

import Model exposing (..)
import MyList as List
import List.Extra as List
import Random
import DeckShuffle exposing (..)
import Http


getUrl url cmd =
    let
        request =
            Http.getString url
    in
        Http.send cmd request



--NewDeck
--NewIndex


csvToSources : String -> List Source
csvToSources text =
    String.split "\n" text
        |> List.map (String.split ",")
        |> List.map List.uncons2
        |> List.flatten
        |> List.map
            (\( name, url, _ ) ->
                Source (String.trim name) (String.trim url)
            )
