module DeckShuffle exposing (..)

import Random
import List.Extra as List
import MyList as List
import Model exposing (..)


shuffleDeckCmd : Model -> Cmd Msg
shuffleDeckCmd model =
    Random.generate DeckShuffled (deckShuffler model)


deckShuffler : Model -> Random.Generator Deck
deckShuffler ({ allCards, waySelector, selectedTags } as model) =
    let
        deck =
            case selectedTags of
                AllTags ->
                    allCards

                Tags tags ->
                    allCards |> List.filter (\c -> c.tags |> List.any (\tag -> tags |> List.member tag))

        deckGenerator =
            List.shuffle deck

        flipGenerator =
            Random.list (List.length deck) Random.bool

        together =
            Random.pair deckGenerator flipGenerator

        zipped : Random.Generator (List ( Card, Bool ))
        zipped =
            together
                |> Random.map
                    (\( cards, flips ) ->
                        (List.zip cards flips)
                    )
    in
        zipped
            |> Random.map
                (\ls ->
                    ls
                        |> List.map
                            (\( c, f ) ->
                                case waySelector of
                                    Both ->
                                        if f then
                                            flip c
                                        else
                                            c

                                    DeutschToFrancais ->
                                        c

                                    FrancaisToDeutsch ->
                                        flip c
                            )
                )


flip : Card -> Card
flip card =
    Card card.back card.front card.tags
