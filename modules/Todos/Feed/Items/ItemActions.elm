module ItemActions
    ( Model, State, Action(Mark, Pin, Snooze, Unsnooze, CollapseSnooze)
    , init, initState
    , state, equalState
    , encode, decoder
    , update, view
    ) where

{-| Module for the actions of an (abstract) item. -}
import Signal
import Html exposing ( Html )
import Effects exposing ( Effects, none )
import Json.Decode as Decode exposing ( Decoder, (:=), object3)
import Json.Encode as Encode exposing ( Value )

import BinaryButton
import SnoozeButton as SB
import BinaryButton as BB

-- MODEL

-- consists of two binary buttons, one for marking and one for pinning an Item
-- and one snooze button for snoozing an item
type alias Model = {mark: BB.Model, pin: BB.Model, snooze: SB.Model}

{-| Initialize this ItemActions, initializing the trhee buttons and batching their effects.-}
init : (Model, Effects Action)
init = initGeneric Nothing

{-| Initialize from state; similarly to init, but values extracted from state -}
initState : State -> (Model, Effects Action)
initState state = initGeneric (Just state)

-- generic init method (not exposed) for avoiding code duplication
initGeneric : Maybe State -> (Model, Effects Action)
initGeneric s =
  let markProps = [BB.TextF "Mark as done", BB.IconF "ok", BB.IconT "remove"]
      pinProps = [BB.TextT "Unpin", BB.TextF "Pin", BB.IconT "pushpin", BB.IconF "pushpin"]
      ((mark, markFx), (pin, pinFx), (snooze, snoozeFx)) =
        case s of
          Just state    -> ( BB.initState markProps state.mark
                           , BB.initState pinProps state.pin
                           , SB.initState state.snooze)
          Nothing       -> ( BB.init markProps
                           , BB.init pinProps
                           , SB.init)
  in ( Model mark pin snooze, Effects.batch [ Effects.map MarkBtn markFx,
                                              Effects.map PinBtn pinFx,
                                              Effects.map SnoozeBtn snoozeFx])

-- STATE

-- saving the state requires the state of the mark, pin and snooze button.
type alias State = { mark: BB.State, pin: BB.State, snooze: SB.State }

{-| Map the given model to its state representation. -}
state : Model -> State
state model = State (BB.state model.mark) (BB.state model.pin) (SB.state model.snooze)

{-| Check if two models have the same state representation. -}
equalState : Model -> Model -> Bool
equalState a b =
  BB.equalState a.mark b.mark &&
  BB.equalState a.pin b.pin &&
  SB.equalState a.snooze b.snooze

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object3 State
  ("mark"   := BB.decoder)
  ("pin"    := BB.decoder)
  ("snooze" := SB.decoder)

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object
  [ ("mark",    BB.encode state.mark)
  , ("pin",     BB.encode state.pin)
  , ("snooze",  SB.encode state.snooze)]


-- UPDATE

type Action = -- Internal (not exposed) nested button actions
                 MarkBtn BB.Action | PinBtn BB.Action | SnoozeBtn SB.Action |
              -- Exposed actions mapping to nested button actions, hiding implementation details
                 Mark | Pin | Snooze | Unsnooze | CollapseSnooze

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    -- Update buttons with nested button actions, effects are mapped.
    MarkBtn a       -> let (mark, fx) = BB.update a model.mark in ({ model | mark <- mark }, Effects.map MarkBtn fx)
    PinBtn a        -> let (pin, fx) = BB.update a model.pin in ({ model | pin <- pin }, Effects.map PinBtn fx)
    SnoozeBtn a     -> let (snooze, fx) = SB.update a model.snooze in ({ model | snooze <- snooze }, Effects.map SnoozeBtn fx)

    -- Exposed actions map to internal nested button actions
    Mark            -> update (MarkBtn BB.Do) model
    Pin             -> update (PinBtn BB.Do) model
    Snooze          -> update (SnoozeBtn SB.Snooze) model
    Unsnooze        -> update (SnoozeBtn SB.Unsnooze) model
    CollapseSnooze  -> update (SnoozeBtn SB.Collapse) model

-- VIEW

view : Signal.Address Action -> Model -> List Html
view address model =
  [ BB.view (Signal.forwardTo address MarkBtn) model.mark,
    BB.view (Signal.forwardTo address PinBtn) model.pin,
    SB.view (Signal.forwardTo address SnoozeBtn) model.snooze ]
