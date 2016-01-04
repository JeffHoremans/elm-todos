module ItemFeedActions
    ( Model, State, Action(HideDoneItems, HideAddReminder)
    , init, initState
    , state, equalState
    , encode, decoder
    , update
    ) where

{-| Module for the actions of the item feed. -}
import Effects exposing (Effects, none)
import Json.Decode as Decode exposing ( Decoder, (:=), string, list, object2, dict )
import Json.Encode as Encode exposing ( Value )

import BinaryButton as BB

-- MODEL

-- consists of a button for hiding done items and for hiden the add reminder functionality
type alias Model = { hideDone: BB.Model, hideAdd: BB.Model }

{-| Initialize the item feed actions to their defaults. Will init two binary buttons.
The hideAdd button will be initialized to true, hiding the add reminder form by default.
Created nested effect will be batched. -}
init : (Model, Effects Action)
init = initGeneric Nothing

{-| Same as for init, but the state of the buttons will be extracted from the given state. -}
initState : State -> (Model, Effects Action)
initState state = initGeneric (Just state)

-- generic init method (not exposed) for avoiding code duplication
initGeneric : Maybe State -> (Model, Effects Action)
initGeneric s =
  let hideDoneProps = [BB.TextT "Unhide", BB.TextF "Hide done"]
      hideAddProps = [BB.TextT "Unhide", BB.TextF "Hide add", BB.StartState True]
      ((hideDone, hideDoneFx), (hideAdd, hideAddFx)) =
        case s of
          Just state  -> ( BB.initState hideDoneProps state.hideDone
                         , BB.initState hideAddProps state.hideAdd )
          Nothing     -> ( BB.init hideDoneProps
                         , BB.init hideAddProps )
  in ( Model hideDone hideAdd, Effects.batch [ Effects.map HideDone hideDoneFx, Effects.map HideAdd hideAddFx ])

-- STATE

type alias State = { hideDone: BB.State, hideAdd: BB.State }

{-| Map the given model to its state representation. -}
state : Model -> State
state model = { hideDone = BB.state model.hideDone, hideAdd = BB.state model.hideAdd }

{-| Check if two models have the same state representation. -}
equalState : Model -> Model -> Bool
equalState a b = BB.equalState a.hideDone b.hideDone && BB.equalState a.hideAdd b.hideAdd

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object2 State
  ("hideDone" := BB.decoder)
  ("hideAdd" := BB.decoder)

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object
  [ ("hideDone",  BB.encode state.hideDone)
  , ("hideAdd",  BB.encode state.hideAdd)]

-- UPDATE

type Action = -- Exposed actions; visible and callable for the outside
                 HideDoneItems | HideAddReminder |
              -- Private actions used internally
                 HideDone BB.Action | HideAdd BB.Action  -- nested binary button actions


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    -- Exposed actions are mapped to internal actions, hiding implementation details
    HideDoneItems -> update (HideDone BB.Do) model
    HideAddReminder -> update (HideAdd BB.Do) model
    -- Update nested binary button actions
    HideDone subAction ->
      let (hideDone, fx) = BB.update subAction model.hideDone
      in (Model hideDone model.hideAdd, Effects.map HideDone fx)
    HideAdd subAction ->
      let (hideAdd, fx) = BB.update subAction model.hideAdd
      in (Model model.hideDone hideAdd, Effects.map HideAdd fx)
