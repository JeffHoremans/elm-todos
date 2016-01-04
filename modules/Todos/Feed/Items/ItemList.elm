module ItemList
    ( Model, State, Action(..)
    , initEmpty, init, initState
    , toList, length
    , state, equalState
    , encode, decoder
    , update, view, elementView ) where

{-| Module representing an ItemList -}
import Signal
import Html exposing ( Html )
import Effects exposing (Effects, none)
import List exposing ( isEmpty, head, tail )
import Json.Decode as Decode exposing ( Decoder, (:=), string, list, object1, object1 )
import Json.Encode as Encode exposing ( Value )

import ListUtils exposing ( contains )
import Item


-- MODEL

-- consists of a list of (uid, item) pairs and the current uid
type alias Model =
    { list : List (Int, Item.Model)
    , uid : Int
    }

{-| Initialize an empty ItemList with an empty list and uid of 0.-}
initEmpty : (Model, Effects Action)
initEmpty = (Model [] 0, none)

{-| Initialize an ItemList with the given list of (initialized) items.
This will initialize an empty ItemList and update it with the AddMany actions,
effectively adding all given items to the ItemList. -}
init : (List (Item.Model, Effects Item.Action), Effects Action) -> (Model, Effects Action)
init list = update (AddMany list) (Model [] 0)

{-| Initialize an ItemList with the given state. Will initialize an ItemList
with the list saved in the state , mapped to initialized items using the
initState methods of Items as the mapping function. -}
initState : State -> (Model, Effects Action)
initState state = init (List.map Item.initState state.list, none)

{-| Retrieve a list of Item Models (so without their uids) from the given model. -}
toList : Model -> List Item.Model
toList model = List.map snd model.list

{-| Get the amount of items the given model contains, the length of the list. -}
length : Model -> Int
length model = List.length model.list

-- STATE

-- to save state only a bare list of item states are needed. Uids are not necessary as they are
-- reset when initializing from state (see initState)
type alias State = { list: List(Item.State) }

{-| Map the given model to its current state. -}
state : Model -> State
state model = State (List.map Item.state (toList model))

{-| Check if two models have the same state representation. -}
equalState : Model -> Model -> Bool
equalState a b =
  length a == length b &&
  List.all (\x -> x == True) (List.map2 Item.equalState (toList a) (toList b))

{-| A JSON decoder for the state. -}
decoder : Decoder State
decoder = object1 State
  ("list" := Decode.list Item.decoder)

{-| JSON-encode the given state. -}
encode : State -> Value
encode state = Encode.object
  [ ("list",  Encode.list (List.map Item.encode state.list))]

-- UPDATE

type Action = Add (Item.Model, Effects Item.Action) |                           -- Add an initialized item to the ItemList
              AddMany (List(Item.Model, Effects Item.Action), Effects Action) | -- Add many initialized items to the ItemList
              Element Int Item.Action                                           -- Nested item action with the specified uid


update : Action -> Model -> (Model, Effects Action)
update message model =
    case message of
        -- Add an (initialized) item to the ItemList
        -- If the ItemList already contains the item (uses the equal method of the Item module)
        -- the item is not added again. Otherwise the item is added to the list with the current uid
        -- and uid is incremented with 1. Its effects are also mapped.
        Add (newItem, fx) -> if not (contains Item.equal newItem (toList model)) then
                              let newModel = Model (model.list ++ [(model.uid, newItem)]) (model.uid + 1)
                              in ( newModel, Effects.map (Element model.uid) fx )
                             else (model, none)
        -- Add many (initialized) items to the ItemList
        AddMany (list, listFx) -> if isEmpty list then (model, listFx)
                        else let (Just item) = head list
                                 (Just items) = tail list
                                 (newModel, fx) = update (Add item) model
                             in update (AddMany (items, Effects.batch[listFx, fx])) newModel
        -- Update an item with the given uid with the given item action
        Element uid action ->
            let subUpdate ((id, item) as entry) =
                    if id == uid then
                        let (newItem, fx) = Item.update action item
                        in ( (id, newItem), Effects.map (Element id) fx )
                    else (entry, Effects.none)
                (newList, fxList) =
                    model.list
                        |> List.map subUpdate
                        |> List.unzip
            in ( { model | list <- newList }, Effects.batch fxList )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model = Html.div [] (List.map (elementView address) model.list)


elementView : Signal.Address Action -> (Int, Item.Model) -> Html
elementView address (id, model) = Item.view (Signal.forwardTo address (Element id)) model
