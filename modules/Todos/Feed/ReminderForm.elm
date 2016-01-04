module ReminderForm
    ( Model, State, Action(Add, Clear)
    , init, initState
    , state, equalState
    , decoder, encode
    , update, view
    ) where

{-| Module representing a form for adding reminders. -}
import Signal
import Time exposing ( Time, second, minute )
import Task exposing ( Task, andThen, onError, succeed )
import TaskTutorial exposing ( getCurrentTime )
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A exposing ( class, href, rel )
import Json.Decode as Decode exposing ( Decoder, (:=), object2 )
import Json.Encode as Encode exposing ( Value )
import Effects exposing ( Effects, none )

import DateUtils exposing ( formatTime, plusDay )
import Reminder

-- MODEL

-- consists of reminder form data and the current time for setting default form date values
type alias Model = { data: Reminder.Data, time: Time }

{-| Initialize the form to the default state. Time is initialized zero but will be updated soon after, see initState. -}
init : (Model, Effects Action)
init = initState (State (Reminder.Data "" (formatTime 0) (Just (formatTime (plusDay 0)))) 0)

{-| Initialize from the given state and call the getTime task as effect. -}
initState : State -> (Model, Effects Action)
initState state = (Model state.data state.time, getTime)

-- STATE

type alias State = { data: Reminder.Data, time: Time }

{-| Map the given model to its state representation. -}
state : Model -> State
state = identity

{-| Check if two models have the same state representation. -}
equalState : Model -> Model -> Bool
equalState a b =
  a.data == b.data &&
  formatTime a.time == formatTime b.time

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object2 State
  ("data" := Reminder.dataDecoder)
  ("time" := Decode.float)

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object
  [ ("data", Reminder.encodeData state.data)
  , ("time", Encode.float state.time) ]

-- UPDATE

type Action = -- Exposed actions usable from the outside
                 Add | Clear |
              -- Internal actions (not exposed)
                 SetDate String | SetBody String | SetDeadline String | -- updating form values
                 Tick Time                                              -- time update

update : Action -> Model -> (Model, Effects Action)
update action model =
  let data = model.data in
  case action of
    -- Add action for alerting feed
    Add                  -> (model, none)
    -- Clear the form, setting back to default
    Clear                -> ({ model | data <- resetData "" model.time}, none)

    -- updating form values
    SetDate date         -> ({ model | data <- { data | created <- date }}, none)
    SetBody body         -> ({ model | data <- { data | body <- body}}, none)
    -- deadline is optional, so if not given set to nothing
    SetDeadline deadline -> ({ model | data <- { data | deadline <- (if deadline == "" then Nothing else Just deadline)}}, none)

    -- time update, if initial update (time == 0) set form date values to values based on the new, right time.
    -- Afterwards call tick task as effects (see below) to update time every second
    Tick time            -> ({ model | time <- time,
                                       data <- (if model.time == 0 then resetData data.body time
                                                else data)}, tick second)

-- reset form date
resetData: String -> Time -> Reminder.Data
resetData body time = Reminder.Data body (DateUtils.formatTime time) (Just (DateUtils.formatTime (DateUtils.plusDay time)))

-- EFFECTS

-- getTime task for requesting the current time and mapping it to the Tick Action
getTime : Effects Action
getTime = getCurrentTime
    |> Task.map Tick
    |> Effects.task

-- tick task that sleeps for a given amount of time and then performs the same action as getTime.
-- Used for updating the time periodically
tick : Time -> Effects Action
tick time = Task.sleep time
        `andThen` (\_ -> getCurrentTime)
          |> Task.map Tick
          |> Effects.task

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.form [A.attribute "class" "form-horizontal"] [
    Html.fieldset [] [
      Html.legend [] [Html.text "Add reminder"],
      Html.div [A.attribute "class" "form-group"] [
        Html.label [A.attribute "class" "col-lg-2 control-label"] [Html.text "Date"],
        Html.div [A.attribute "class" "col-lg-10"] [
          Html.input [A.attribute "class" "form-control"
          , A.attribute "type" "date",
                      E.on "input" E.targetValue (Signal.message address << SetDate), A.value model.data.created, A.type' "date"] []]],
      Html.div [A.attribute "class" "form-group"] [
        Html.label [A.attribute "class" "col-lg-2 control-label"] [Html.text "Body"],
        Html.div [A.attribute "class" "col-lg-10"] [
          Html.textarea [ A.attribute "class" "form-control", A.attribute "rows" "3",
                          E.on "input" E.targetValue (Signal.message address << SetBody), A.value model.data.body, A.type' "text"] []]],
      Html.div [A.attribute "class" "form-group"] [
        Html.label [A.attribute "class" "col-lg-2 control-label"] [Html.text "Deadline ", Html.small [] [Html.text "(optional)"]],
        Html.div [A.attribute "class" "col-lg-10"] [
          Html.input [A.attribute "class" "form-control", A.attribute "type" "date",
                      E.on "input" E.targetValue (Signal.message address << SetDeadline), A.value (Maybe.withDefault "" model.data.deadline), A.type' "date"] []]],
      Html.div [A.attribute "class" "form-group"] [
        Html.div [E.onClick address Add, A.attribute "class" "col-lg-10 col-lg-offset-2"] [
          Html.a [A.attribute "class" "btn btn-primary"] [Html.text "Add"]]]]]
