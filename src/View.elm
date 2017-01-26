module View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)


view model =
    div [ class "container" ]
        [ selectorsView model [ AllWays, DeutschToFrancais, FrancaisToDeutsch ]
        , cardView model
        , div [ class "row" ]
            [ button [ onClick OnPrevious, class "button" ] [ text "Previous" ]
            , button [ onClick ToggleSolution, class "button" ]
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


cardView model =
    case model.currentCard of
        Nothing ->
            text "No card to display"

        Just card ->
            let
                cardText =
                    String.append card.front <|
                        if model.showSolution then
                            String.append " → " card.back
                        else
                            ""
            in
                div [ class "row" ]
                    [ div [ class "twelve columns" ]
                        [ h4 [] [ text cardText ]
                        ]
                    ]


selectorsView model selectors =
    div [ class "row" ]
        (selectors
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


selectorString : Selector -> String
selectorString sel =
    case sel of
        AllWays ->
            "Both"

        DeutschToFrancais ->
            "DE → FR"

        FrancaisToDeutsch ->
            "FR → DE"
