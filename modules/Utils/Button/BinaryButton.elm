module BinaryButton
    ( Model, Property(..), State, Action(..)
    , init, initState
    , state, equalState
    , encode, decoder
    , update, view
    ) where

{-| A module representing a generic button with a binary state.
It is quite customizable as its text, class and icon can be configured
for both of its states.
 -}
import Html exposing ( Html )
import Html.Attributes as A
import Html.Events as E
import Effects exposing ( Effects, none )
import Json.Decode as Decode exposing ( Decoder, (:=), string, list, object1, dict )
import Json.Encode as Encode exposing ( Value )
import List exposing ( isEmpty, head, tail )

-- MODEL

-- has a binary state and values for its text, class and icon for both state values:
type alias Model = { state: Bool
                   , textTrue: String,      textFalse: String
                   , classTrue: String,     classFalse: String
                   , iconClassTrue: String, iconClassFalse: String }

-- union type used to facilitate initialization (see init below)
type Property = TextT String | TextF String | ClassT String | ClassF String | IconT String | IconF String | StartState Bool

{-| Init a binary button with the given properties. Any properties given will be set, all properties
that were omitted will be initialized to their defaults. Uses the initState method to avoid code duplication.
By default the button will be initialized with the false state, default class and no icon.

    init [] == initState [] (State False) == (Model False "Undo" "Do" "btn-default" "btn-default" "" "", none)
    init [TextT "True", TextFalse "False", ClassT "btn-danger"] ==
    initState [TextT "True", TextFalse "False", ClassT "btn-danger"] (State False) ==
    (Model False "True" "False" "btn-danger" "btn-default" "" "", none)

-}
init: List Property -> (Model, Effects Action)
init properties = initState properties (State False)

{-| Init a binary button with the given properties from a saved state and no effects. Any properties given will be set, all properties
that were omitted will be initialized to their defaults (see init).

    initState [] (State False) == (Model False "Undo" "Do" "btn-default" "btn-default" "" "", none)
    initState [TextT "True", TextFalse "False", ClassT "btn-danger"] (State False) == (Model False "True" "False" "btn-danger" "btn-default" "" "", none)

-}
initState: List Property -> State -> (Model, Effects Action)
initState properties state =
  let model = Model state.state "Undo" "Do" "btn-default" "btn-default" "" ""
  in (setProperties properties model, none)

-- helper method (not exposed) for setting properties recursively to the given model
setProperties: List Property -> Model -> Model
setProperties properties model =
  if isEmpty properties then model
  else
    let (Just property) = head properties
        (Just rest)     = tail properties
    in setProperties rest (setProperty property model)

-- helper method (not exposed) for setting a property to the given model
setProperty: Property -> Model -> Model
setProperty property model =
  case property of
    TextT s ->      { model | textTrue <- s }
    TextF s ->      { model | textFalse <- s }
    ClassT s ->     { model | classTrue <- s }
    ClassF s ->     { model | classFalse <- s }
    IconT s ->      { model | iconClassTrue <- s }
    IconF s ->      { model | iconClassFalse <- s }
    StartState s -> { model | state <- s }
    _ -> model

-- STATE

-- saving the state of a binary button only requires its binary state:
type alias State = { state: Bool }

{-| Map the given model to its state representation. -}
state : Model -> State
state model = State model.state

{-| Check if two models have the same state representation. -}
equalState : Model -> Model -> Bool
equalState a b = a.state == b.state

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object1 State
  ("state" := Decode.bool)

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object
  [ ("state",  Encode.bool state.state)]

-- UPDATE

type Action = Do

-- the only action complements the state of the button
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Do -> ({ model | state <- not model.state}, none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let text      = if model.state then model.textTrue      else model.textFalse
      class     = if model.state then model.classTrue     else model.classFalse
      iconClass = if model.state then model.iconClassTrue else model.iconClassFalse
  in
    Html.button [E.onClick address Do, A.attribute "class" ("btn " ++ class) ]
                [ Html.span [A.attribute "class" ("glyphicon glyphicon-" ++ iconClass)] []
                , Html.text (" " ++ text)]
