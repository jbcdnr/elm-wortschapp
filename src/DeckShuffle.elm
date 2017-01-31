module DeckShuffle exposing (..)

import Random
import List.Extra as List
import MyList as List
import Model exposing (..)
import Char


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
                    entries |> List.map .data |> List.map (applySelector selector)

                Nothing ->
                    []
    in
        Random.map convert deckGenerator


applySelector : Selector -> List String -> Card
applySelector selector data =
    let
        interpolation : String -> List String
        interpolation str =
            case List.elemIndex '$' (String.toList str) of
                Nothing ->
                    [ str ]

                Just index ->
                    let
                        before =
                            String.left index str

                        dollarCount =
                            String.dropLeft (index + 1) str

                        value =
                            List.takeWhile Char.isDigit (String.toList dollarCount)

                        rest =
                            List.dropWhile Char.isDigit (String.toList dollarCount)
                    in
                        before :: String.fromList ('$' :: value) :: interpolation (String.fromList rest)

        format : String -> List String -> String
        format str args =
            str
                |> interpolation
                |> List.map
                    (\w ->
                        if String.startsWith "$" w then
                            String.dropLeft 1 w |> String.toInt |> Result.toMaybe |> Maybe.andThen (\i -> List.getAt (i - 1) args) |> Maybe.withDefault w
                        else
                            w
                    )
                |> String.join " "
    in
        Card (format selector.front data) (format selector.back data)
