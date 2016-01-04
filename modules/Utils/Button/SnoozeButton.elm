module SnoozeButton
    ( Model, State, Action(Collapse, Snooze, Unsnooze)
    , init, initState
    , state, equalState
    , encode, decoder
    , update, view
    ) where

{-| A module representing a snooze button. -}
import Html exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Effects exposing ( Effects, none )
import Time exposing ( Time )
import Task exposing ( andThen )
import TaskTutorial exposing ( getCurrentTime )
import Json.Decode as Decode exposing ( Decoder, (:=), object4)
import Json.Encode as Encode exposing ( Value )
import Date

import BinaryButton as BB
import DateUtils

-- MODEL

-- A snooze button consists of a nested button for collapsing a small form,
-- a binary state representing being snoozed or not, form data (in this case the due date)
-- and finally the current time (in milliseconds) for setting and updating the due date field data.
type alias Model = { btn: BB.Model
                   , state: Bool
                   , due: String
                   , time: Time }

{-| Init the snooze button to its defaults; a simple button for toggling the snooze form is configured, its effects nested.
The snooze state is set to False, the time to zero and the due date empty.
The effects that are run are the nested effects of the configured button and the getTime task for updating the time (see getTime).

    init == let (btn, btnFx) = BinaryButton.init [BB.TextT "Cancel", BB.TextF "Snooze", BB.IconF "bell", BB.IconT ""]
            in  (Model btn False  time, Effects.batch [getTime, Effects.map CollapseBtn fx])

 -}
init : (Model, Effects Action)
init =  initGeneric Nothing

{-| Init the snooze button from the given saved state. Same as init with properties taken from the given
state instead as the defaults. -}
initState : State -> (Model, Effects Action)
initState state = initGeneric (Just state)

-- generic init method (not exposed) for avoiding code duplication
initGeneric : Maybe State -> (Model, Effects Action)
initGeneric s =
  let props = [BB.TextT "Cancel", BB.TextF "Snooze", BB.IconF "bell", BB.IconT ""]
      ((btn, fx), state, due, time) =
        case s of
          Just state  -> (BB.initState props state.btn, state.state, state.due, state.time)
          Nothing     -> (BB.init props, False, "", 0)
  in ( Model btn state due time
     , Effects.batch [getTime, Effects.map CollapseBtn fx])

{-| Check if past snooze due -}
isPastSnoozeDue : Model -> Bool
isPastSnoozeDue model =
  let deadline =
    (case Date.fromString model.due of
      Ok date -> Date.toTime date
      _ -> 0)
  in deadline `DateUtils.isBefore` model.time || deadline `DateUtils.isEqual` model.time

-- STATE

-- saving its state requires the state of the nested button, its state, its form data (due) and the current time
type alias State = { btn: BB.State, state: Bool, due: String, time: Time }

{-| Map the given model to its state representation. -}
state : Model -> State
state model = State (BB.state model.btn) model.state model.due model.time

{-| Check if two models have the same state representation -}
equalState : Model -> Model -> Bool
equalState a b =
  BB.equalState a.btn b.btn &&
  a.state == b.state &&
  a.due == b.due &&
  DateUtils.formatTime a.time == DateUtils.formatTime b.time

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object4 State
  ("btn"    := BB.decoder)
  ("state"  := Decode.bool)
  ("due"    := Decode.string)
  ("time"   := Decode.float)

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object
  [ ("btn",   BB.encode state.btn)
  , ("state", Encode.bool state.state)
  , ("due",   Encode.string state.due)
  , ("time",  Encode.float state.time)]


-- UPDATE

type Action =  -- Exposed actions, usable for the outside
                  Collapse | Snooze |
               -- Internal actions (not exposed)
                  Unsnooze |                              -- unsnooze
                  CollapseBtn BB.Action |                 -- nested binary button action
                  SetDate String |                        -- form update
                  Tick Time                               -- time update



update : Action -> Model -> (Model, Effects Action)
update action model = case action of

    -- Map exposed Collapse action to hidden nested CollapseBtn action (hides implementation)
    Collapse -> update (CollapseBtn BB.Do) model
    CollapseBtn subAction -> let (btn, fx) = BB.update subAction model.btn in ({ model | btn <- btn }, Effects.map CollapseBtn fx)
    Snooze -> let (btn, fx) = BB.update BB.Do model.btn in  ({ model | btn <- btn, state <- True}, Effects.map CollapseBtn fx)
    Unsnooze -> ({ model | state <- False, due <- (DateUtils.formatTime (DateUtils.plusDay model.time))}, none)

    -- Update form value: due date
    SetDate due -> ({ model | due <- due }, none)

    -- time update; first check if snoozed en past snooze due.
    -- if so, update with the Unsnooze action, else leave be.
    -- In any of those cases, if the first (time == 0) update form value as well,
    -- afterwards call tick task as effects (see below) to update time every second
    Tick time -> let (newModel, fx) =
                    if model.state && isPastSnoozeDue model then update Unsnooze model
                    else (model, none)
                 in  ({ newModel | time <- time,
                                   due <- if newModel.time == 0 then (DateUtils.formatTime (DateUtils.plusDay time)) else newModel.due }, tick)

-- EFFECTS

-- getTime task for requesting the current time and mapping it to the Tick Action
getTime : Effects Action
getTime =
  getCurrentTime
    |> Task.map Tick
    |> Effects.task

-- tick task that sleeps for a second and then performs the same action as getTime. Used for updating the time periodically (every second)
tick : Effects Action
tick = Task.sleep (Time.second) `andThen` (\_ -> getCurrentTime)
          |> Task.map Tick
          |> Effects.task

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.btn.state of
    False -> BB.view (Signal.forwardTo address CollapseBtn) model.btn
    True -> Html.div [A.attribute "class" "form-group"] [
              Html.label [A.attribute "class" "control-label"] [Html.text "Snooze until: "],
              Html.div [A.attribute "class" "input-group"] [
                Html.span [A.attribute "class" "input-group-btn"] [BB.view (Signal.forwardTo address CollapseBtn) model.btn],
                Html.input [A.attribute "class" "form-control", A.attribute "type" "date", A.attribute "autofocus" "",
                            E.on "input" E.targetValue (Signal.message address << SetDate), A.value model.due, A.type' "date"][],
                Html.span [A.attribute "class" "input-group-btn"] [
                  Html.button [E.onClick address Snooze, A.attribute "class" ("btn btn-default")] [Html.text "Ok"]]]]
