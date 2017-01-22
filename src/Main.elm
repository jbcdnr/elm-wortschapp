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
    | PickNewCard Card
    | ToggleHelp
    | NewDeck (Result Http.Error String)


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
    }


defaultModel =
    Model
        []
        (Card "" "")
        False


view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "twelve columns" ]
                [ h2 [] [ text model.currentCard.front ]
                , if model.showSolution then
                    h4 [] [ text model.currentCard.back ]
                  else
                    text ""
                ]
            ]
        , div [ class "row" ]
            [ div [ class "six columns" ]
                [ button [ onClick ToggleHelp ]
                    [ text
                        (if model.showSolution then
                            "Hide"
                         else
                            "Show"
                        )
                    ]
                ]
            , div [ class "six columns" ]
                [ button [ onClick OnNext, class "button-primary" ] [ text "Next" ] ]
            ]
        ]


flip : Card -> Card
flip card =
    Card card.back card.front


randomCardPicker : Deck -> Random.Generator Card
randomCardPicker deck =
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
                    in
                        if f then
                            flip card
                        else
                            card
                )


pickCardCmd : Deck -> Cmd Msg
pickCardCmd deck =
    Random.generate PickNewCard (randomCardPicker deck)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnNext ->
            ( model, pickCardCmd model.deck )

        PickNewCard card ->
            ( { model | currentCard = card, showSolution = False }, Cmd.none )

        ToggleHelp ->
            ( { model | showSolution = not model.showSolution }, Cmd.none )

        NewDeck (Ok newDeck) ->
            let
                deck =
                    csvToDeck newDeck
            in
                ( { model | deck = deck }, pickCardCmd deck )

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
        |> List.map (String.split "\t")
        |> List.map extractFirstPair
        |> flatten
        |> List.map
            (\c ->
                let
                    ( a, b ) =
                        c
                in
                    Card a b
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
