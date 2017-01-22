module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List


main =
    beginnerProgram { model = defaultModel, view = view, update = update }


type alias Card =
    { front : String
    , back : String
    , id : Int
    }


type alias Model =
    { deck : List Card
    , currentCard : Int
    , showSolution : Bool
    }


defaultModel =
    Model
        [ Card "Hallo" "Bonjour" 1
        , Card "Danke" "Merci" 2
        , Card "Bitte" "Thanks" 3
        ]
        1
        False


cardWithId : Int -> List Card -> Maybe Card
cardWithId id cards =
    List.find (\c -> c.id == id) cards


view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "twelve columns" ]
                [ h2 [] [ text (cardWithId model.currentCard model.deck |> Maybe.map .front |> Maybe.withDefault "") ]
                , if model.showSolution then
                    h4 [] [ text (cardWithId model.currentCard model.deck |> Maybe.map .back |> Maybe.withDefault "") ]
                  else
                    text ""
                ]
            ]
        , div [ class "row" ]
            [ div [ class "four columns" ]
                [ button [ onClick OnKnown ] [ text "I know" ] ]
            , div [ class "four columns" ]
                [ button [ onClick OnUnknown ] [ text "I don't know" ] ]
            , div [ class "four columns" ]
                [ button [ onClick ToggleHelp ]
                    [ text
                        (if model.showSolution then
                            "Hide"
                         else
                            "Show"
                        )
                    ]
                ]
            ]
        ]


type Msg
    = OnKnown
    | OnUnknown
    | ToggleHelp


getIdAfter : Int -> List Card -> Int
getIdAfter id cards =
    List.zip cards (List.append (List.drop 1 cards) (List.take 1 cards))
        |> List.find
            (\c ->
                let
                    ( a, b ) =
                        c
                in
                    a.id == id
            )
        |> Maybe.map
            (\c ->
                let
                    ( a, b ) =
                        c
                in
                    b.id
            )
        |> Maybe.withDefault id


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnKnown ->
            { model | currentCard = getIdAfter model.currentCard model.deck }

        OnUnknown ->
            { model | currentCard = getIdAfter model.currentCard model.deck }

        ToggleHelp ->
            { model | showSolution = not model.showSolution }
