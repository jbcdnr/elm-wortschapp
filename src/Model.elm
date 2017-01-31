module Model exposing (..)

import Http
import List.Extra as List
import MyList as List


type Msg
    = OnNext
    | OnPrevious
    | ToggleSolution
    | DeckShuffled Deck
    | ChangeSelector Selector
    | ToggleTag Tag
    | ToggleAllTag
    | NewDeck (Result Http.Error String)
    | NewIndex (Result Http.Error String)
    | ChangeSource String
    | NoOp


type alias Tag =
    String


type alias Source =
    { name : String
    , url : String
    }


type alias Deck =
    List Card


type alias Entry =
    { data : List String
    , tags : List Tag
    }


type alias Card =
    { front : String
    , back : String
    }


type alias Model =
    { sources : List Source
    , allEntries : List Entry
    , currentCard : Maybe Card
    , previousCards : Deck
    , nextCards : Deck
    , showSolution : Bool
    , selectedSelector : Maybe Selector
    , selectors : List Selector
    , selectedTags : SelectedTags
    }


type SelectedTags
    = AllTags
    | Tags (List Tag)


defaultModel =
    { sources = []
    , allEntries = []
    , currentCard = Nothing
    , previousCards = []
    , nextCards = []
    , showSolution = False
    , selectedSelector = Nothing
    , selectors = []
    , selectedTags = AllTags
    }


tags : Model -> List Tag
tags model =
    model.allEntries
        |> List.flatMap .tags
        |> List.filter ((/=) "")
        |> List.unique
        |> List.sort


type alias Selector =
    { name : String
    , apply : List String -> Card
    }
