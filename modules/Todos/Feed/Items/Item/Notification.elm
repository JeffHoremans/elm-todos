module Notification
    ( Data, Model, State, Action(..)
    , init, initState
    , state, equalState
    , encode, decoder
    , encodeData, dataDecoder
    , update, view
    ) where

{-| Module representing a notification. -}
import Html exposing ( Html )
import Html.Attributes as A
import Effects exposing ( Effects, none )
import Signal
import Json.Decode as Decode exposing ( Decoder, (:=), object1, object4)
import Json.Encode as Encode exposing ( Value )

-- MODEL

-- Notification data
type alias Data =
  { from: String
  , source: String
  , content: String
  , date: String
  }

-- consists of an icon and its data. Possible future reminder specific actions are supported.
type alias Model = { actions: {}, icon: String, data: Data }

{-| Initialize a notification with the given data. The icon is set to "user". -}
init : Data -> (Model, Effects Action)
init data = initState (State data)

{-| Initialize a notification from state. Similarly to init, but with data extracted from state.-}
initState : State -> (Model, Effects Action)
initState state = ( Model {} "user" state.data, none)

-- STATE

-- storing state requires its data
type alias State = { data: Data }

{-| Map the given model to its state representation. -}
state : Model -> State
state model = State model.data

{-| Check if two models have the same state representation.-}
equalState : Model -> Model -> Bool
equalState a b = a.data == b.data

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object1 State
  ("data" := dataDecoder)

{-| A JSON decoder for notification data -}
dataDecoder : Decoder Data
dataDecoder = object4 Data
  ("from"     := Decode.string)
  ("source"   := Decode.string)
  ("content"  := Decode.string)
  ("date"     := Decode.string)

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object
  [ ("data", encodeData state.data) ]

{-| JSON-encode the given notification data -}
encodeData : Data -> Value
encodeData state = Encode.object
  [ ("from"   , Encode.string state.from)
  , ("source" , Encode.string state.source)
  , ("content", Encode.string state.content)
  , ("date"   , Encode.string state.date) ]


-- UPDATE

type Action = NoOp  -- do nothing

-- currently no actions are supported
update : Action -> Model -> (Model, Effects Action)
update action model = (model, none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [] [
    Html.h4 [A.attribute "class" "list-group-item-heading"] [Html.text (model.data.from ++ " via " ++ model.data.source)],
    Html.p [A.attribute "class" "list-group-item-text"] [Html.text (model.data.content)],
    Html.br [] []]
