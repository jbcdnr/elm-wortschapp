module Model exposing (..)

import Http


type Msg
    = OnNext
    | OnPrevious
    | ToggleSolution
    | PickNewCard Card
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
    , showSolution : Bool
    , waySelector : Selector
    }


type Selector
    = Both
    | DeutschToFrancais
    | FrancaisToDeutsch
