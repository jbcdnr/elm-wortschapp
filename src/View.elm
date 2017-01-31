module View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Json.Decode as Json
import Html.Events.Extra exposing (targetValueIntParse)


view model =
    div [ class "container" ]
            [ h5 [] [ text "Source" ]
            , select [ onChange ChangeSource ] (model.sources |> List.map (\s -> option [] [ text s.name ]))
            , h5 [] [ text "Categories" ]
            , (tagsView model)
            , h5 [] [ text "Order" ]
            , selectorsView model [ Both, DeutschToFrancais, FrancaisToDeutsch ]
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
        )


onChange tagger =
    on "change" (Json.map tagger targetValue)


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


tagsView model =
    let
        isSelected tag =
            case model.selectedTags of
                AllTags ->
                    False

                Tags tags ->
                    tags |> List.member tag

        selectedAllButton =
            case model.selectedTags of
                AllTags ->
                    True

                Tags ls ->
                    False

        allButton =
            tagView "All" selectedAllButton ToggleAllTag "#2ecc71"
    in
        div [ class "row" ] (allButton :: (tags model |> List.map (\tag -> tagView tag (isSelected tag) (ToggleTag tag) "#2ecc71")))


selectorsView model selectors =
    div [ class "row" ]
        (selectors
            |> List.map
                (\s -> tagView (selectorString s) (s == model.waySelector) (ChangeSelector s) "#bdc3c7")
        )


tagView label selected onClickCmd selectedColor =
    let
        tagStyle =
            if selected then
                [ ( "color", "white" ), ( "background-color", selectedColor ), ( "border-color", selectedColor ) ]
            else
                []
    in
        span
            [ onClick onClickCmd
            , class "tag"
            , style tagStyle
            ]
            [ text label ]


selectorString : Selector -> String
selectorString sel =
    case sel of
        Both ->
            "Both"

        DeutschToFrancais ->
            "DE → FR"

        FrancaisToDeutsch ->
            "FR → DE"
