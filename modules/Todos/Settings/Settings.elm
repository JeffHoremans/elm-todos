module Settings
    ( Model, State, Action(..)
    , init, initState
    , themes
    , state, equalState
    , decoder, encode
    , update, view
    ) where

{-| Settings module for the todos module -}
import Signal
import Html exposing ( Html )
import Html.Events
import Html.Attributes as A exposing ( class, href, rel )
import Json.Decode as Decode exposing ( Decoder, (:=), object1 )
import Json.Encode as Encode exposing ( Value )
import Effects exposing ( Effects, none )

import ListUtils exposing ( getAt )
import List exposing ( map )

-- MODEL

-- consists of a theme
type alias Model = { theme: String }

{-| Initialize with the default theme. -}
init : (Model, Effects Action)
init = ( Model "default", none)

{-| Initialize from a saved state. -}
initState : State -> (Model, Effects Action)
initState state = (state, none)

-- all supported themes (got from http://bootswatch.com/)
themes : List (String, String)
themes = [  ("Default", "default")
          , ("Cerulean", "cerulean")
          , ("Simplex", "simplex")
          , ("Slate", "slate")
          , ("United", "united")
          , ("Yeti", "yeti") ]

-- get the theme corresponding at the given index
getTheme : Int -> String
getTheme index = let (Just theme) = themes `getAt` index in snd theme

-- STATE

-- saving the state of the settings requires the theme:
type alias State = { theme: String }

{-| Map the given model to its state representation. -}
state : Model -> State
state = identity

{-| Check if two models have the same state representation. -}
equalState : Model -> Model -> Bool
equalState a b = a.theme == b.theme

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object1 State
  ("theme" := Decode.string)

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object [ ("theme", Encode.string state.theme) ]

-- UPDATE

type Action = SelectTheme Int

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SelectTheme index -> ({ model | theme <- (getTheme index)}, none)

-- VIEW

-- custom decoder for getting the selected index
targetSelectedIndex : Decoder Int
targetSelectedIndex = Decode.at ["target", "selectedIndex"] Decode.int

viewThemes : Model -> List Html
viewThemes model = map (\theme -> Html.option [A.selected (model.theme == snd theme)] [Html.text (fst theme)]) themes

view : Signal.Address Action -> Model -> Html
view address model =
  let selectEvent = Html.Events.on "change" targetSelectedIndex (Signal.message address << SelectTheme)
  in Html.div [] [
      Html.h3 [] [Html.text "Change theme: "],
      Html.select [class "form-control", selectEvent] (viewThemes model)]
