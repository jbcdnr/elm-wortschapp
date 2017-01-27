module Model exposing (..)

import Http


type Msg
    = OnNext
    | OnPrevious
    | ToggleSolution
    | DeckShuffled Deck
    | ChangeSelector Selector
    | NewDeck (Result Http.Error String)


type alias Deck =
    List Card


type alias Card =
    { front : String
    , back : String
    }


type alias Model =
    { allCards : Deck
    , currentCard : Maybe Card
    , previousCards : Deck
    , nextCards : Deck
    , showSolution : Bool
    , waySelector : Selector
    }


defaultModel =
    { allCards = []
    , currentCard = Nothing
    , previousCards = []
    , nextCards = []
    , showSolution = False
    , waySelector = Both
    }


type Selector
    = Both
    | DeutschToFrancais
    | FrancaisToDeutsch
