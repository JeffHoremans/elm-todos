module Email
    ( Data, Model, State, Action(Collapse)
    , init, initState
    , state, equalState
    , encode, decoder
    , encodeData, dataDecoder
    , update, view
    ) where

{-| Module representing a email. -}
import Html exposing ( Html )
import Html.Attributes as A
import Effects exposing ( Effects, none )
import Signal
import String
import Json.Decode as Decode exposing ( Decoder, (:=), object1, object2, object5)
import Json.Encode as Encode exposing ( Value )

import BinaryButton as BB

-- MODEL

-- Email data
type alias Data =
  { from: String
    , to: String
    , title: String
    , body: String
    , date: String
    }

-- consists of an icon, its data and a collapse actions, truncating its body when large enough.
type alias Model =
  { actions: { collapse: BB.Model }
   , icon: String
   , data: Data
   }

{-| Initialize an email based on the given data.
initializes a binary button for the collapse functionality, sets the data
and sets the icon to "inbox". -}
init : Data -> (Model, Effects Action)
init data = let (collapse, fx) = BB.init [ BB.TextT "Less",
                                            BB.TextF "More",
                                            BB.ClassT "btn-default btn-xs",
                                            BB.ClassF "btn-default btn-xs" ]
            in (Model { collapse= collapse } "inbox" data, Effects.map CollapseBtn fx)

{-| Initialize an email from state. Similarly to init, but with data and collapse state from state.-}
initState : State -> (Model, Effects Action)
initState state = let (collapse, fx) = BB.initState [ BB.TextT "Less",
                                                 BB.TextF "More",
                                                 BB.ClassT "btn-default btn-xs",
                                                 BB.ClassF "btn-default btn-xs" ] state.actions.collapse
             in (Model { collapse= collapse } "inbox" state.data, Effects.map CollapseBtn fx)


-- STATE
-- storing state requires its data and the current state of the collapse button
type alias State = { actions: { collapse: BB.State },  data: Data }

{-| Map the given model to its state representation. -}
state : Model -> State
state model = State { collapse= BB.state model.actions.collapse } model.data

{-| Check if two models have the same state representation. -}
equalState : Model -> Model -> Bool
equalState a b = a.data == b.data && BB.equalState a.actions.collapse b.actions.collapse

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object2 State
  ("actions" :=
    (object1 (\x -> { collapse= x })
      ("collapse" := BB.decoder)))
  ("data" := dataDecoder)

{-| A JSON decoder for email data -}
dataDecoder : Decoder Data
dataDecoder = object5 Data
  ("from"   := Decode.string)
  ("to"     := Decode.string)
  ("title"  := Decode.string)
  ("body"   := Decode.string)
  ("date"   := Decode.string)

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object
  [ ("actions", Encode.object [("collapse", BB.encode state.actions.collapse)])
  , ("data", encodeData state.data)]

{-| JSON-encode the given email data -}
encodeData : Data -> Value
encodeData state = Encode.object
  [ ("from" , Encode.string state.from)
  , ("to"   , Encode.string state.to)
  , ("title", Encode.string state.title)
  , ("body" , Encode.string state.body)
  , ("date" , Encode.string state.date) ]

-- UPDATE

type Action = -- Internal nested binary button actions
                 CollapseBtn BB.Action |
              -- Exposed actions mapping to nested binary button actions
                 Collapse

update : Action -> Model -> (Model, Effects Action)
update action model =
  let modelActions = model.actions in
    case action of
      -- Update binary button using nested binary button actions
      CollapseBtn subAction -> let (collapse, fx) = BB.update subAction model.actions.collapse in ({ model | actions <- { modelActions | collapse <- collapse }}, Effects.map CollapseBtn fx)
      -- Exposed collapse action mapped to nested binary button action
      Collapse -> update (CollapseBtn BB.Do) model

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [] (List.concat
    [ [ Html.h4 [A.attribute "class" "list-group-item-heading"] [Html.text (model.data.title ++ " | " ++ model.data.from)],
       Html.p [A.attribute "class" "list-group-item-text"] [ Html.text (if model.actions.collapse.state then model.data.body else (truncate model.data.body))]]
    , (if needsTruncation model.data.body then
      [BB.view (Signal.forwardTo address CollapseBtn) model.actions.collapse]
      else []),
    [ Html.br [] [],
      Html.br [] [] ] ])

{-| Check if the given string needs truncation (its length being greater than 200 characters) -}
needsTruncation : String -> Bool
needsTruncation text = String.length text > 200

{-| Truncate the given string. -}
truncate : String -> String
truncate text = String.slice 0 197 text ++ "..."
