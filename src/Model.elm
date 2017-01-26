module Model exposing (..)

import Http


type Msg
    = OnNext
    | OnPrevious
    | ToggleHelp
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
    { deck : Deck
    , currentCard : Card
    , showSolution : Bool
    , previous : Deck
    , waySelector : Selector
    }


type Selector
    = AllWays
    | DeutschToFrancais
    | FrancaisToDeutsch
