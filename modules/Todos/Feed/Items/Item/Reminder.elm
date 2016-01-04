module Reminder
    ( Data, Model, State, Action(..)
    , init, initState
    , state, equalState
    , encode, decoder
    , encodeData, dataDecoder
    , update, view
    ) where

{-| Module representing a reminder. -}
import Html exposing ( Html )
import Html.Attributes as A
import Signal
import Date exposing ( Date, fromString, toTime )
import Effects exposing ( Effects, none )
import Task exposing ( andThen )
import TaskTutorial exposing ( getCurrentTime )
import Time exposing ( Time )
import Json.Decode as Decode exposing ( Decoder, (:=), object2, object3)
import Json.Encode as Encode exposing ( Value )

import DateUtils


-- MODEL

-- Reminder data, deadline is optional, so its a maybe value
type alias Data =
  { body: String
  , created: String
  , deadline: Maybe String
  }

-- consists of an icon, its data, the current time and possible future reminder specific actions are supported.
type alias Model =
   { actions: {}
   , icon: String
   , data: Data
   , time: Time
   }

{-| Initialize a reminder based on the given data with the time set to zero and the "time" icon.
The getTime task is passed as an effect to update the time periodically -}
init : Data -> (Model, Effects Action)
init data = initState (State data 0)

{-| Initialize a reminder from state. Similarly to init, but with data and time from state.-}
initState : State -> (Model, Effects Action)
initState state = ( Model {} "time" state.data state.time, getTime )

-- STATE

-- storing state requires its data and the current time
type alias State = { data: Data, time: Time }

{-| Map the given model to its state representation. -}
state : Model -> State
state model = State model.data model.time

{-| Check if two models have the same state representation.
The time is compared based on its date string representation to prevent
to much localstorage operations (see todos module and documentation in Main.elm) -}
equalState : Model -> Model -> Bool
equalState a b =
  a.data == b.data &&
  DateUtils.formatTime a.time == DateUtils.formatTime b.time

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object2 State
  ("data" := dataDecoder)
  ("time" := Decode.float)

{-| A JSON decoder for reminder data -}
dataDecoder : Decoder Data
dataDecoder = object3 Data
  ("body"     := Decode.string)
  ("created"  := Decode.string)
  (Decode.maybe ("deadline" := Decode.string))

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object
  [ ("data", encodeData state.data)
  , ("time", Encode.float state.time) ]

{-| JSON-encode the given reminder data -}
encodeData : Data -> Value
encodeData state =
  let main = [ ("body",     Encode.string state.body)
             , ("created",  Encode.string state.created) ]
  in case state.deadline of
    Just deadline -> Encode.object (List.append main [("deadline", Encode.string deadline)])
    _ -> Encode.object main

-- UPDATE

type Action = Tick Time -- time update

update : Action -> Model -> (Model, Effects Action)
update action model = case action of
    -- update the time and call the tick task as an effect to keep periodically updating the time
    Tick time -> ( { model | time <- time }, tick )

{-| Check if the reminder is past its due date. -}
isPastDue: String -> Model -> Bool
isPastDue deadline model =
  let d = case fromString deadline of
    Ok date -> toTime date
    _ -> 0
  in d `DateUtils.isBefore` model.time

-- EFFECTS

-- getTime task for requesting the current time and mapping it to the Tick Action
getTime : Effects Action
getTime = getCurrentTime
    |> Task.map Tick
    |> Effects.task

-- tick task that sleeps for a second and then performs the same action as getTime. Used for updating the time periodically (every second)
tick : Effects Action
tick = Task.sleep Time.second
  `andThen` (\_ -> getCurrentTime)
      |> Task.map Tick
      |> Effects.task

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [] (List.append
    [Html.h4 [A.attribute "class" "list-group-item-heading"] [Html.text model.data.body]]
    (case model.data.deadline of
      Just deadline -> [ Html.p [A.class (if isPastDue deadline model then "text-danger" else "")] [Html.text ("Due: " ++ deadline)] ]
      _ -> []))
