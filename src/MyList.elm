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
