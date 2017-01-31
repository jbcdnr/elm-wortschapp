module MyList exposing (..)

import List.Extra as List
import Random


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


flatMap : (a -> List b) -> List a -> List b
flatMap f ls =
    ls
        |> List.map f
        |> List.foldr List.append []


uncons2 : List a -> Maybe ( a, a, List a )
uncons2 ls =
    case List.uncons ls of
        Nothing ->
            Nothing

        Just ( first, rest ) ->
            case List.uncons rest of
                Nothing ->
                    Nothing

                Just ( second, rest ) ->
                    Just ( first, second, rest )


uncons3 : List a -> Maybe ( a, a, a, List a )
uncons3 ls =
    case ( List.getAt 0 ls, List.getAt 1 ls, List.getAt 2 ls, List.drop 3 ls ) of
        ( Just a, Just b, Just c, others ) ->
            Just ( a, b, c, others )

        others ->
            Nothing


shuffle : List a -> Random.Generator (List a)
shuffle list =
    let
        randomIndexes =
            Random.list (List.length list) (Random.float 0 1)

        zipped =
            randomIndexes |> Random.map (List.zip list)

        ordered =
            zipped |> Random.map (List.sortBy (\( a, b ) -> b))

        clean =
            ordered |> Random.map (List.map (\( a, b ) -> a))
    in
        clean
