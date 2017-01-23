module Main exposing (..)

import Html exposing (..)
import Http exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Random
import Http


main =
    Html.program
        { init = ( defaultModel, getDeck )
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


type Msg
    = OnNext
    | OnPrevious
    | ToggleHelp
    | PickNewCard Card
    | ChangeSelector Selector
    | NewDeck (Result Http.Error String)


type Selector
    = AllWays
    | DeutschToFrancais
    | FrancaisToDeutsch


selectorString : Selector -> String
selectorString sel =
    case sel of
        AllWays ->
            "Both"

        DeutschToFrancais ->
            "DE → FR"

        FrancaisToDeutsch ->
            "FR → DE"


type alias Deck =
    List Card


type alias Card =
    { front : String
    , back : String
    }


type alias Model =
    { deck : Deck
    , currentCard : Card
    , showSolution : Bool
    , previous : Deck
    , waySelector : Selector
    }


defaultModel =
    Model
        []
        (Card "" "")
        False
        []
        AllWays


cardView : Card -> Bool -> String
cardView card withSolution =
    String.append card.front <|
        if withSolution then
            String.append " → " card.back
        else
            ""


view model =
    div [ class "container" ]
        [ div [ class "row" ]
            ([ AllWays, DeutschToFrancais, FrancaisToDeutsch ]
                |> List.map
                    (\s ->
                        button
                            [ onClick (ChangeSelector s)
                            , class
                                (if s == model.waySelector then
                                    "button-primary selector"
                                 else
                                    "selector"
                                )
                            ]
                            [ text (selectorString s) ]
                    )
            )
        , div [ class "row" ]
            [ div [ class "twelve columns" ]
                [ h4 [] [ text <| cardView model.currentCard model.showSolution ]
                ]
            ]
        , div [ class "row" ]
            [ button [ onClick OnPrevious, class "button" ] [ text "Previous" ]
            , button [ onClick ToggleHelp, class "button" ]
                [ text
                    (if model.showSolution then
                        "Hide"
                     else
                        "Show"
                    )
                ]
            , button [ onClick OnNext, class "button button-primary" ] [ text "Next" ]
            ]
        ]


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
                                AllWays ->
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

        ToggleHelp ->
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


getDeck : Cmd Msg
getDeck =
    let
        url =
            "https://dl.dropboxusercontent.com/s/6h82np562bctp36/voc.csv?dl=0"

        request =
            Http.getString url
    in
        Http.send NewDeck request


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
