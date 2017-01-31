module DeckShuffle exposing (..)

import Random
import List.Extra as List
import MyList as List
import Model exposing (..)


shuffleDeckCmd : Model -> Cmd Msg
shuffleDeckCmd model =
    Random.generate DeckShuffled (deckShuffler model)


deckShuffler : Model -> Random.Generator Deck
deckShuffler ({ allEntries, selectedSelector, selectedTags } as model) =
    let
        deck =
            case selectedTags of
                AllTags ->
                    allEntries

                Tags tags ->
                    allEntries |> List.filter (\c -> c.tags |> List.any (\tag -> tags |> List.member tag))

        deckGenerator : Random.Generator (List Entry)
        deckGenerator =
            List.shuffle deck

        convert : List Entry -> List Card
        convert entries =
            case selectedSelector of
                Just selector ->
                    entries |> List.map .data |> List.map selector.apply

                Nothing ->
                    []
    in
        Random.map convert deckGenerator
