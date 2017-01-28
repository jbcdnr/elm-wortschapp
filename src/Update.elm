module Update exposing (..)

import List.Extra as List
import MyList as List
import Model exposing (..)
import DeckShuffle exposing (..)
import Cmd exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ allCards, previousCards, nextCards, showSolution, waySelector, selectedTags } as model) =
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
                    ( { model | currentCard = Nothing, previousCards = [], nextCards = [] }, Cmd.none )

        OnNext ->
            withCurrentCard model
                (\currentCard ->
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
                )

        OnPrevious ->
            withCurrentCard model
                (\currentCard ->
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
                )

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

        ToggleTag tag ->
            let
                newTags =
                    case selectedTags of
                        AllTags ->
                            Tags [ tag ]

                        Tags tags ->
                            if List.member tag tags then
                                if List.length tags == 1 then
                                    AllTags
                                else
                                    Tags (List.remove tag tags)
                            else
                                Tags (tag :: tags)

                newModel =
                    { model | selectedTags = newTags, currentCard = Nothing, previousCards = [], nextCards = [] }
            in
                ( newModel, shuffleDeckCmd newModel )

        ToggleAllTag ->
            let
                newSelectedTags =
                    case selectedTags of
                        AllTags ->
                            Tags []

                        Tags ls ->
                            AllTags

                newModel =
                    { model | selectedTags = newSelectedTags, currentCard = Nothing, previousCards = [], nextCards = [] }
            in
                ( newModel, shuffleDeckCmd newModel )

        NoOp ->
            ( model, Cmd.none )


withCurrentCard : Model -> (Card -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
withCurrentCard model update =
    case model.currentCard of
        Nothing ->
            ( model, Cmd.none )

        Just c ->
            update c
