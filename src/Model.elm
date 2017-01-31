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


type alias Card =
    { front : String
    , back : String
    , tags : List Tag
    }


type alias Model =
    { sources : List Source
    , allCards : Deck
    , currentCard : Maybe Card
    , previousCards : Deck
    , nextCards : Deck
    , showSolution : Bool
    , waySelector : Selector
    , selectedTags : SelectedTags
    }


type SelectedTags
    = AllTags
    | Tags (List Tag)


defaultModel =
    { sources = []
    , allCards = []
    , currentCard = Nothing
    , previousCards = []
    , nextCards = []
    , showSolution = False
    , waySelector = Both
    , selectedTags = AllTags
    }


tags : Model -> List Tag
tags model =
    model.allCards
        |> List.flatMap .tags
        |> List.unique
        |> List.sort


type Selector
    = Both
    | DeutschToFrancais
    | FrancaisToDeutsch
