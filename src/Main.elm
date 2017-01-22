module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Random


main =
    Html.program
        { init = ( defaultModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }


type alias Card =
    { front : String
    , back : String
    }


type alias Model =
    { deck : List Card
    , currentCard : Card
    , showSolution : Bool
    }


defaultModel =
    Model
        [ Card "Hallo" "Bonjour" 1
        , Card "Danke" "Merci" 2
        , Card "Bitte" "Thanks" 3
        ]
        (Card "Bitte" "Thanks" 3)
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


type Msg
    = OnNext
    | PickNewCard Card
    | ToggleHelp


randomCardPicker : List Card -> Random.Generator Card
randomCardPicker deck =
    Random.int 0 (List.length deck - 1) |> Random.map (\i -> List.getAt i deck |> Maybe.withDefault (Card "" "" -1))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnNext ->
            ( model, Random.generate PickNewCard (randomCardPicker model.deck) )

        PickNewCard card ->
            ( { model | currentCard = card, showSolution = False }, Cmd.none )

        ToggleHelp ->
            ( { model | showSolution = not model.showSolution }, Cmd.none )
