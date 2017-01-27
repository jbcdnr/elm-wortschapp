module Update exposing (..)

import List.Extra as List
import MyList as List
import Model exposing (..)
import DeckShuffle exposing (..)
import Cmd exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ allCards, previousCards, nextCards, showSolution, waySelector } as model) =
    case msg of
        NewDeck (Ok newDeck) ->
            let
                deck =
                    csvToDeck newDeck

                newModel =
                    { model | allCards = deck }
            in
                ( newModel, shuffleDeckCmd newModel )

        NewDeck (Err error) ->
            ( model, Cmd.none )

        DeckShuffled deck ->
            case List.uncons deck of
                Just ( first, others ) ->
                    let
                        newPreviousCards =
                            case model.currentCard of
                                Just c ->
                                    c :: previousCards

                                Nothing ->
                                    previousCards
                    in
                        ( { model
                            | currentCard = Just first
                            , previousCards = newPreviousCards
                            , nextCards = others
                            , showSolution = False
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

        otherMsg ->
            case model.currentCard of
                Nothing ->
                    ( model, Cmd.none )

                Just currentCard ->
                    case otherMsg of
                        OnNext ->
                            case List.uncons nextCards of
                                Just ( head, tail ) ->
                                    let
                                        newModel =
                                            { model
                                                | currentCard = Just head
                                                , nextCards = tail
                                                , previousCards = currentCard :: previousCards
                                                , showSolution = False
                                            }
                                    in
                                        ( newModel, Cmd.none )

                                Nothing ->
                                    ( model, shuffleDeckCmd model )

                        OnPrevious ->
                            let
                                newModel =
                                    case List.uncons previousCards of
                                        Nothing ->
                                            model

                                        Just ( first, others ) ->
                                            { model
                                                | previousCards = others
                                                , currentCard = Just first
                                                , nextCards = currentCard :: nextCards
                                                , showSolution = False
                                            }
                            in
                                ( newModel, Cmd.none )

                        ChangeSelector selector ->
                            if selector == waySelector then
                                ( model, Cmd.none )
                            else
                                let
                                    newModel =
                                        { model
                                            | waySelector = selector
                                            , previousCards = []
                                            , currentCard = Nothing
                                        }
                                in
                                    ( newModel
                                    , shuffleDeckCmd newModel
                                    )

                        ToggleSolution ->
                            ( { model | showSolution = not showSolution }, Cmd.none )

                        -- Not very nice
                        others ->
                            ( model, Cmd.none )
