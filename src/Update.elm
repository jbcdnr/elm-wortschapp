module Update exposing (..)

import List.Extra as List
import MyList as List
import Model exposing (..)
import DeckShuffle exposing (..)
import Cmd exposing (..)


uncons3 : List a -> Maybe ( a, a, a )
uncons3 ls =
    case ( List.getAt 0 ls, List.getAt 1 ls, List.getAt 2 ls ) of
        ( Just a, Just b, Just c ) ->
            Just ( a, b, c )

        others ->
            Nothing




createDeck : String -> ( List Entry, List Selector )
createDeck rawFile =
    let
        rows =
            String.split "\n" rawFile

        commands =
            List.filter (String.startsWith ":") rows

        length =
            commands
                |> List.find (String.startsWith ":length")
                |> Maybe.andThen (\r -> String.split "," r |> List.map String.trim |> List.getAt 1)
                |> Maybe.andThen (\l -> String.toInt l |> Result.toMaybe)
                |> Maybe.withDefault 2

        selectors =
            commands
                |> List.filter (String.startsWith ":selector")
                |> List.map (\r -> String.split "," r |> List.map String.trim)
                |> List.map (List.drop 1)
                |> List.map uncons3
                |> List.flatten
                |> List.map (\( name, front, back ) -> Selector name front back)

        entries =
            List.filter (\r -> not (String.startsWith ":" r)) rows

        createEntry : Int -> String -> Maybe Entry
        createEntry length row =
            let
                values =
                    String.split "," row |> List.map String.trim
            in
                if List.length values < length then
                    Nothing
                else
                    Just (Entry (List.take length values) (List.drop length values |> List.filter ((/=) "")))

        allEntries =
            entries |> List.map (createEntry length) |> List.flatten
    in
        ( allEntries, selectors )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ allEntries, previousCards, nextCards, showSolution, selectedSelector, selectedTags } as model) =
    case msg of
        NewDeck (Ok newDeck) ->
            let
                ( allEntries, selectors ) =
                    createDeck newDeck

                newModel =
                    { model | allEntries = allEntries, selectors = selectors, selectedSelector = List.head selectors }
            in
                ( newModel, shuffleDeckCmd newModel )

        NewDeck (Err error) ->
            ( model, Cmd.none )

        NewIndex (Ok newIndex) ->
            let
                sources =
                    csvToSources newIndex

                newModel =
                    { model | sources = sources }

                cmd =
                    case List.uncons sources of
                        Nothing ->
                            Cmd.none

                        Just ( head, tail ) ->
                            getDeck head.url
            in
                ( newModel, cmd )

        NewIndex (Err error) ->
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
            if Just selector == model.selectedSelector then
                ( model, Cmd.none )
            else
                let
                    newModel =
                        { model
                            | selectedSelector = Just selector
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

        ChangeSource name ->
            case List.find (\s -> s.name == name) model.sources of
                Nothing ->
                    ( model, Cmd.none )

                Just { url } ->
                    ( { model | currentCard = Nothing, previousCards = [], nextCards = [] }, getDeck url )

        NoOp ->
            ( model, Cmd.none )


withCurrentCard : Model -> (Card -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
withCurrentCard model update =
    case model.currentCard of
        Nothing ->
            ( model, Cmd.none )

        Just c ->
            update c
