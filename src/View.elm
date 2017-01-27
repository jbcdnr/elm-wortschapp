module View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)


view model =
    div [ class "container" ]
        [ selectorsView model [ Both, DeutschToFrancais, FrancaisToDeutsch ]
        , cardView model
        , div [ class "row" ]
            [ button [ onClick OnPrevious, class "button four columns" ] [ text "Previous" ]
            , button [ onClick ToggleSolution, class "button four columns" ]
                [ text
                    (if model.showSolution then
                        "Hide"
                     else
                        "Show"
                    )
                ]
            , button [ onClick OnNext, class "button button-primary four columns" ] [ text "Next" ]
            ]
        ]


cardView model =
    case model.currentCard of
        Nothing ->
            div [ class "row" ]
                [ div [ class "twelve columns" ]
                    [ h4 [] [ text "No card to display" ]
                    ]
                ]

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
                    span
                        [ onClick (ChangeSelector s)
                        , class <|
                            String.append
                                (if s == model.waySelector then
                                    "selected"
                                 else
                                    ""
                                )
                                " selector"
                        ]
                        [ text (selectorString s) ]
                )
        )


selectorString : Selector -> String
selectorString sel =
    case sel of
        Both ->
            "Both"

        DeutschToFrancais ->
            "DE → FR"

        FrancaisToDeutsch ->
            "FR → DE"
