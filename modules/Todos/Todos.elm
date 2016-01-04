module Todos
    ( Model, Action
    , init, update, view
    , shortcuts
    ) where

{-| Module for the todos application. -}
import Signal
import Html exposing ( Html )
import Html.Attributes as A exposing (class, href, rel)
import Html.Events as E
import Effects exposing ( Effects, none )
import Json.Encode as Encode exposing ( Value )
import Json.Decode as Decode exposing ( Decoder, object2, string, list, (:=) )
import Storage exposing (..)
import Task exposing ( Task, andThen, onError, succeed )

import ItemFeed
import Settings
import Item
import Static
import List
import Reminder

-- MODEL

-- consists of an item-feed and a settings module
type alias Model = { feed: ItemFeed.Model, settings: Settings.Model }

{-| Initialize this todos module by initializing the feed and settings module.
Their effects are ignored because we call the getState task as an effect to initialize
the feed and the settings from a possible saved state from localstorage (see below). -}
init : (Model, Effects Action)
init = let  (feed, _) = ItemFeed.init []
            (settings, _) = Settings.init
       in ( Model feed settings
          , Effects.batch [ getState ] )

{-| Import items from the static context. Different kind of items are mapped to items by their respective constructors
    and concatenated to one list of items. As the static reminder data does not contain the optional deadline property,
    and additional map is needed, setting the deadline property to Nothing (Maybe value).
-}
importItems : List (Item.Model, Effects Item.Action)
importItems =
  List.concat
    [ (List.map Item.reminder (List.map (\x -> Reminder.Data x.body x.created Nothing) Static.reminders))
    , (List.map Item.email Static.emails)
    , (List.map Item.notification Static.notifications) ]

-- STATE

-- the state of the todos module consists of the state of the settings and the feed module respectively
type alias State = { settings: Settings.State, feed: ItemFeed.State }

{-| Map the given model to its state representation. -}
currentState : Model -> State
currentState model = { settings = Settings.state model.settings, feed = ItemFeed.state model.feed }

{-| A JSON decoder for the state -}
decoder : Decoder State
decoder = object2 State
  ("settings" := Settings.decoder)
  ("feed"     := ItemFeed.decoder)

{-| JSON-encode the given state -}
encode : State -> Value
encode state = Encode.object
  [ ("settings",  Settings.encode state.settings)
  , ("feed",      ItemFeed.encode state.feed)]

-- UPDATE

type Action = Feed ItemFeed.Action | Settings Settings.Action | -- nested feed and settings actions
              GetState (Maybe State) |                          -- receiving possible saved state
              NoOp                                              -- No operation; doing nothing

              | ClearStorage

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    -- updating nested feed actions. Only if the state of the updated feed is different (not equal) from
    -- the original, we call the storeState task as an effect which stores the currentState in localStorage (see below)
    -- Note that while the states of the original and the updated one can be equal, their models are not necessarily.
    -- Time for instance is updated very often, but is only considered changed in state when it results in a different date.
    Feed subAction ->
      let (feed, fx)  = ItemFeed.update subAction model.feed
          feedChanged = not (ItemFeed.equalState model.feed feed)
          newModel    = { model | feed <- feed }
      in (newModel, Effects.batch (List.append
                                    [Effects.map Feed fx]
                                    (if feedChanged then [storeState (currentState newModel)] else [])))
    -- updating nested settings actions. Similarly to the feed, only if the state of updated settings is different from the original,
    -- we call the storeState task.
    Settings subAction ->
      let (settings, fx)  = Settings.update subAction model.settings
          settingsChanged = not (Settings.equalState model.settings settings)
          newModel        = { model | settings <- settings }
      in (newModel, Effects.batch (List.append
                                    [Effects.map Settings fx]
                                    (if settingsChanged then [storeState (currentState newModel)] else [])))

    -- Initializing from a possible saved state (Maybe value). If so, the feed and setting are initialize from
    -- their respective states and their effects nested, if not they are normally initialized (and their effects nested).
    -- The feed, in this last case, is initialized with items from the Static module (see importItems).
    GetState s -> case s of
      Just st ->
        let (feed, feedFx) = ItemFeed.initState st.feed
            (settings, fx) = Settings.initState st.settings
        in ( { model | feed <- feed, settings <- settings }
           , Effects.batch [ Effects.map Feed feedFx, Effects.map Settings fx ])
      _ ->
        let  (feed, feedFx) = ItemFeed.init importItems
             (settings, fx) = Settings.init
        in ( { model | feed <- feed, settings <- settings }
           , Effects.batch [ Effects.map Feed feedFx, Effects.map Settings fx ] )
    NoOp -> (model, none)


    ClearStorage -> (model, Effects.batch [clearState, getState])


-- EFFECTS

clearStorage : Task String ()
clearStorage = clear

clearState : Effects Action
clearState = clearStorage
  |> Task.toMaybe
  |> Task.map (\_ -> NoOp)
  |> Effects.task


{-| Store the given state to localstorage.
Using third-party elm-storage package: https://github.com/TheSeamau5/elm-storage/
announced on: http://elm-lang.org/blog/announce/0.15
Operation is wrapped into empty Task.
Uses the encode method for encoding the given state.
-}
store : State -> Task String ()
store = setItem "appstate" << encode

{-| Get the state stored in localstorage
Using third-party elm-storage package: https://github.com/TheSeamau5/elm-storage/
announced on: http://elm-lang.org/blog/announce/0.15
Operation is wrapped into empty Task.
Uses the decoder method for decoding the retrieved state.
-}
get : Task String State
get = getItem "appstate" decoder

{-| Store the given state to localstorage (by calling store task defined above)
and mapping it to an effects action task.  As something could have gone wrong
with the localstorage operations, the result needs to be mapped into a maybe value to remain pure.
We then map to the NoOp action, as nothing needs to change.
-}
storeState : State -> Effects Action
storeState state = store state
    |> Task.toMaybe
    |> Task.map (\_ -> NoOp)
    |> Effects.task

{-| Get the given state to localstorage (by calling get task defined above)
and mapping it to an effects action task containing the retrieved state. As something could have gone wrong
with the localstorage operations, the result needs to be mapped into a maybe value to remain pure.
We than map to the GetState action to initialize.
-}
getState: Effects Action
getState = get
    |> Task.toMaybe
    |> Task.map GetState
    |> Effects.task

-- INPUTS

-- Map the shortcuts from the feed. Possible shortcuts from other nested modules (e.g settings) could be easily merged
shortcuts : Signal Action
shortcuts = Signal.map Feed ItemFeed.shortcuts

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model = Html.body [] [
                  Html.node "link" [href ("assets/css/" ++ model.settings.theme ++ ".css"), rel "stylesheet"] [],
                  Html.button [A.class "btn btn-default", E.onClick address ClearStorage] [Html.text "Clear storage"],
                  Html.div [class "container"]
                      [ Html.div [class "row"]
                          [ Html.div [class "col-lg-7"] [ItemFeed.view (Signal.forwardTo address Feed) model.feed]
                          , Html.div [class "col-lg-5"] [Html.h2 [] [Html.text "Settings"]
                                                         , Settings.view (Signal.forwardTo address Settings) model.settings]]]]
