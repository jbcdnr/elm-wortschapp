module DeckShuffle exposing (..)

import Random
import List.Extra as List
import MyList as List
import Model exposing (..)


shuffleDeckCmd : Model -> Cmd Msg
shuffleDeckCmd ({ allCards, waySelector } as model) =
    Random.generate DeckShuffled (deckShuffler allCards waySelector)


deckShuffler : Deck -> Selector -> Random.Generator Deck
deckShuffler deck sideSelector =
    let
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
                                case sideSelector of
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
    Card card.back card.front
