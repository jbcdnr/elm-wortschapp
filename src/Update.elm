module Update exposing (..)

import List.Extra as List
import Random
import Model exposing (..)
import Http


flip : Card -> Card
flip card =
    Card card.back card.front


randomCardPicker : Deck -> Selector -> Random.Generator Card
randomCardPicker deck sideSelector =
    let
        cardGenerator =
            Random.int 0 (List.length deck - 1)
                |> Random.map (\i -> List.getAt i deck |> Maybe.withDefault (Card "" ""))

        flipGenerator =
            Random.bool

        together =
            Random.pair cardGenerator flipGenerator
    in
        together
            |> Random.map
                (\c ->
                    let
                        ( card, f ) =
                            c

                        shouldFlip =
                            case sideSelector of
                                Both ->
                                    f

                                DeutschToFrancais ->
                                    False

                                FrancaisToDeutsch ->
                                    True
                    in
                        if shouldFlip then
                            flip card
                        else
                            card
                )


pickCardCmd : Model -> Cmd Msg
pickCardCmd model =
    Random.generate PickNewCard (randomCardPicker model.deck model.waySelector)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnNext ->
            ( model, pickCardCmd model )

        OnPrevious ->
            let
                newModel =
                    case List.uncons model.previous of
                        Nothing ->
                            model

                        Just ( first, others ) ->
                            { model | previous = others, currentCard = first }
            in
                ( newModel, Cmd.none )

        ChangeSelector selector ->
            let
                newModel =
                    { model | waySelector = selector }

                cmd =
                    if selector /= model.waySelector then
                        pickCardCmd newModel
                    else
                        Cmd.none
            in
                ( newModel, cmd )

        PickNewCard card ->
            ( { model | currentCard = card, showSolution = False, previous = model.currentCard :: model.previous }, Cmd.none )

        ToggleSolution ->
            ( { model | showSolution = not model.showSolution }, Cmd.none )

        NewDeck (Ok newDeck) ->
            let
                deck =
                    csvToDeck newDeck

                newModel =
                    { model | deck = deck, previous = [] }
            in
                ( newModel, pickCardCmd newModel )

        NewDeck (Err error) ->
            ( { model | currentCard = Card (toString error) "" }, Cmd.none )


csvToDeck : String -> Deck
csvToDeck text =
    String.split "\n" text
        |> List.map (String.split ",")
        |> List.map extractFirstPair
        |> flatten
        |> List.map
            (\c ->
                let
                    ( a, b ) =
                        c
                in
                    Card (String.trim a) (String.trim b)
            )


extractFirstPair : List a -> Maybe ( a, a )
extractFirstPair ls =
    case List.uncons ls of
        Nothing ->
            Nothing

        Just ( first, rest ) ->
            case List.uncons rest of
                Nothing ->
                    Nothing

                Just ( second, rest ) ->
                    Just ( first, second )


flatten : List (Maybe a) -> List a
flatten ls =
    let
        reducer =
            \elem acc ->
                case elem of
                    Nothing ->
                        acc

                    Just x ->
                        x :: acc
    in
        List.foldl reducer [] ls |> List.reverse
