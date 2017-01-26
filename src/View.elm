module View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)


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


selectorString : Selector -> String
selectorString sel =
    case sel of
        AllWays ->
            "Both"

        DeutschToFrancais ->
            "DE → FR"

        FrancaisToDeutsch ->
            "FR → DE"


cardView : Card -> Bool -> String
cardView card withSolution =
    String.append card.front <|
        if withSolution then
            String.append " → " card.back
        else
            ""
