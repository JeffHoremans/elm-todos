module ItemFeed
    ( Model, State, Action(..)
    , init, initState, size
    , state, equalState
    , decoder, encode
    , shortcuts
    , update, view
    ) where

{-| Module representing an item feed, the heart of the todos application. -}
import Effects exposing ( Effects, none )
import Time exposing ( Time, second, minute )
import Task exposing ( Task, andThen, onError, succeed )
import Html exposing ( Html )
import Html.Attributes as A
import Http
import Json.Decode as Decode exposing ( Decoder, (:=), string, list, object1, object4, object5, dict )
import Json.Encode as Encode exposing ( Value )

import ItemList
import ReminderForm
import ItemFeedActions
import Item
import Email
import ListUtils exposing (uniq, getAt)
import Shortcut exposing ( alt )

-- MODEL

-- union type representing the sorting strategy, could be easily extended
type SortBy = PinDate | DateReverse

-- consists of an item-list, a reminder form, item-feed actions, a sorting strategy and the index of the currently focused item
type alias Model = { items: ItemList.Model
                   , focusIndex: Int
                   , sort: SortBy
                   , form: ReminderForm.Model
                   , actions: ItemFeedActions.Model }

{-| Initialize an item feed with the given list of initialized items (and their initial effects).
The initial focusIndex is zero (first element), default sorting strategy is PinDate and the other
nested modules are initialized and their effects batched.-}
init : List (Item.Model, Effects Item.Action) -> (Model, Effects Action)
init items = initGeneric Nothing items

{-| Initialize from state; similarly to init, but values extracted from state -}
initState : State -> (Model, Effects Action)
initState state = initGeneric (Just state) []

-- generic init method (not exposed) for avoiding code duplication
initGeneric : Maybe State -> List (Item.Model, Effects Item.Action) -> (Model, Effects Action)
initGeneric s itms =
  let ((items, itemsFx), (actions, actionsFx), (form, formFx),focusIndex) =
    case s of
      Just state  -> ( ItemList.initState state.items
                     , ItemFeedActions.initState state.actions
                     , ReminderForm.initState state.form
                     , state.focusIndex)
      Nothing     -> ( ItemList.init (itms, none)
                     , ItemFeedActions.init
                     , ReminderForm.init
                     , 0)
  in ( Model items focusIndex PinDate form actions
     , Effects.batch [ getEmails, Effects.map Items itemsFx, Effects.map Actions actionsFx, Effects.map Form formFx ])

{-| Size of the VISIBLE feed, snoozed items or hidden done items are not considered. -}
size : Model -> Int
size model = (List.length (todo model)) + (if model.actions.hideDone.state then 0 else (List.length (done model)))

-- STATE

-- saving state requires its items, the focus index, the form data and the state of its actions.
-- the sorting strategy is not needed in the current implementation as it only changes when holding a shortcut,
-- but if other sorting strategies are implemented that can remain for an amount of time, it will also need to
-- be included in the state
type alias State = { items: ItemList.State
                   , focusIndex: Int
                   , form: ReminderForm.State
                   , actions: ItemFeedActions.State }

{-| Map the given model to its state representation. -}
state : Model -> State
state model = { items       = ItemList.state model.items
              , focusIndex  = model.focusIndex
              , form        = ReminderForm.state model.form
              , actions     = ItemFeedActions.state model.actions }

{-| Check if two models have the same state representation. For nested modules use their equalState methods -}
equalState : Model -> Model -> Bool
equalState a b =
  ItemList.equalState a.items b.items &&
  a.focusIndex == b.focusIndex &&
  ReminderForm.equalState a.form b.form &&
  ItemFeedActions.equalState a.actions b.actions

{-| A JSON decoder for the state; for nested modules use their decoder methods. -}
decoder : Decoder State
decoder = object4 State
  ("items"        := ItemList.decoder)
  ("focusIndex"   := Decode.int)
  ("reminderForm" := ReminderForm.decoder)
  ("actions"      := ItemFeedActions.decoder)

{-| JSON-encode the given state; for nested modules use their encode methods. -}
encode : State -> Value
encode state = Encode.object
  [ ("items",         ItemList.encode state.items)
  , ("focusIndex",    Encode.int state.focusIndex)
  , ("reminderForm",  ReminderForm.encode state.form)
  , ("actions",       ItemFeedActions.encode state.actions)]

-- UPDATE

type Action = -- Internal nested module actions
                 Items ItemList.Action | Actions ItemFeedActions.Action | Form ReminderForm.Action |

              -- Exposed actions that map to ItemList- (and nested Item-) actions
                 Truncate | Mark | Pin | CollapseSnooze | Snooze |
              -- Exposed actions that map to ItemFeedActions actions
                 HideDoneItems | HideAddReminder |
              -- Exposed actions specific for the feed
                 FocusNext | FocusPrevious | Sort SortBy |                     -- update focused index / sorting strategy

              -- Internal actions specific for the feed
                 GetEmails (List Email.Data) |                                 -- recieve emails (periodically)
                 NoOp                                                          -- do nothing


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    -- Nested module actions for the nested ItemList, ItemFeedActions and ReminderForm actions
    -- Its nested effects are batched.
    Items subAction   -> let (items, fx)    = ItemList.update subAction model.items in ({ model | items <- items }, Effects.map Items fx)
    Actions subAction -> let (actions, fx)  = ItemFeedActions.update subAction model.actions in ({model | actions <- actions}, Effects.map Actions fx)
    -- When the nested ReminderForm subAction is the ReminderForm.Add action, the form is being submitted.
    -- We thus retrieve the form data from the reminderForm, add a new item to the ItemList with the data and clear the form.
    -- In the other case, we just update the ReminderForm with its subAction as we would normally do.
    Form subAction    -> case subAction of
                          ReminderForm.Add -> let (items, itemsFx) = ItemList.update (ItemList.Add (Item.reminder model.form.data)) model.items
                                                  (form, formFx)  = ReminderForm.update ReminderForm.Clear model.form
                                              in ( { model | items <- items, form <- form}
                                                 , Effects.batch [Effects.map Items itemsFx, Effects.map Form formFx])
                          _ -> let (form, fx)  = ReminderForm.update subAction model.form in ({model | form <- form}, Effects.map Form fx)

    -- Nested ItemList actions that update items in the list (and thereby feed)
    -- The getFocusedElementId method maps the focused index in the feed to its actual list id in the ItemList.
    Truncate          -> update (Items (ItemList.Element (getFocusedElementId model) Item.Truncate)) model
    Pin               -> update (Items (ItemList.Element (getFocusedElementId model) Item.Pin)) model
    Mark              -> update (Items (ItemList.Element (getFocusedElementId model) Item.Mark)) model
    CollapseSnooze    -> update (Items (ItemList.Element (getFocusedElementId model) Item.CollapseSnooze)) model
    Snooze            -> update (Items (ItemList.Element (getFocusedElementId model) Item.Snooze)) model
    -- Nested ItemFeedActions actions
    -- When hiding 'done' items an edge case involving the focusing can occur if the focus was on a 'done' item.
    -- We call the checkHiddenFocus method which updates the focusIndex if need be.
    HideDoneItems     -> let (newModel,fx) = update (Actions (ItemFeedActions.HideDoneItems)) model in (checkHiddenFocus newModel, fx)
    HideAddReminder   -> update (Actions (ItemFeedActions.HideAddReminder)) model
    -- Updating focusIndex (see methods below)
    FocusNext         -> (focusNext model, none)
    FocusPrevious     -> (focusPrevious model, none)
    -- Updating sorting strategy
    Sort by           -> ({ model | sort <- by }, none)

    -- Recieve fetched emails; update the ItemList by calling the AddMany action which adds all the emails.
    -- Not that when no emails are present or something went wrong during the request, the resulting list of Emails
    -- will be empty and thus no emails will be added. As a consequence no additional check is needed here.
    -- As an efffect, we call the checkEmails task which sleeps for a minute before fetching emails again.
    GetEmails emails -> let (items, fx) = ItemList.update (ItemList.AddMany ((List.map Item.email emails), none)) model.items
                        in ({ model | items <- items }, Effects.batch [Effects.map Items fx, checkEmails (minute)])
    -- Do nothing
    NoOp -> (model,none)

{-| Map the currently focused index to the actual corresponding uid in the ItemList.
To achieve this, we split and sort the ItemList the same way the view does and get the corresponding
uid of the at the focused index. Note that we do not consider snoozed items and hidden done items as well.
It thereby prevents actions being performed on hidden items. -}
getFocusedElementId : Model -> Int
getFocusedElementId model =
  let todoList    = todo model    |> sort (comparison model)
      doneList    = if model.actions.hideDone.state then [] else done model    |> sort (comparison model)
      list        = List.append todoList doneList
  in case list `getAt` model.focusIndex of
    Just (id, _) -> id
    _ -> -1

{-| Check for the edge case that the focus was on a 'done' item prior to hidden all 'done' items.
This results in the focusIndex being greater or equal than the size of the feed (which ignores hidden 'done' items).
If this is the case, together with the 'done' items being hidden, we focus the next item.
This could appear weird at first, but the focusNext method uses the size of the feed. The current focusIndex being out of
bounds results in the focusNext method to update the focusIndex to the top (in this case the first todo item). -}
checkHiddenFocus : Model -> Model
checkHiddenFocus model =
  if model.focusIndex >= size model && model.actions.hideDone.state then
    focusNext model
  else model

{-| Focus the next visible item index. If we've reached the bottom, we go to the top,
if we've reached the top we go to the bottom. -}
focusNext : Model -> Model
focusNext model = if (model.focusIndex+1) >= size model then {model | focusIndex <- 0}
                  else {model | focusIndex <- model.focusIndex+1}

{-| Similar to focusNext but focusing the previous item index. -}
focusPrevious : Model -> Model
focusPrevious model = if model.focusIndex-1 < 0 then {model | focusIndex <- size model-1}
  else {model | focusIndex <- model.focusIndex-1 }

{-| Sort a list of items based on the given sorting function. -}
sort : ((Int, Item.Model) -> (Int, Item.Model) -> Order) -> List (Int, Item.Model) -> List (Int, Item.Model)
sort comp list = List.sortWith comp list

{-| Get the appropriate sorting function corresponding to the current sorting strategy -}
comparison : Model -> ((Int, Item.Model) -> (Int, Item.Model) -> Order)
comparison model =
  case model.sort of
    PinDate -> defaultComparison
    DateReverse -> dateReverseComparison

{-| The default sorting function. First prefers pinned items, if an item is pinned
and the other one not, the pinned item is always preferred. Secondly sort by date, so when
two items are being pinned they compare based on their respective dates. -}
defaultComparison : (Int, Item.Model) -> (Int, Item.Model) -> Order
defaultComparison (_,a) (_,b) =
  if (Item.isPinned a) && (not (Item.isPinned b)) then LT
  else if(Item.isPinned b) && (not (Item.isPinned a)) then GT
  else compare (Item.date a) (Item.date b)

{-| Sorting function to sort by date reversed does not considers an item being pinned or not. -}
dateReverseComparison : (Int, Item.Model) -> (Int, Item.Model) -> Order
dateReverseComparison (_,a) (_,b) = compare (Item.date b) (Item.date a)

{-| Filter out all items that are in the 'todo' state and are not snoozed. -}
todo : Model -> List (Int, Item.Model)
todo model = List.filter (\(id, item) -> not (Item.isSnoozed item) && not (Item.isDone item)) model.items.list

{-| Filter out all items that are in the 'done' state and are not snoozed. -}
done : Model -> List (Int, Item.Model)
done model = List.filter (\(id, item) -> not (Item.isSnoozed item) && Item.isDone item) model.items.list

-- EFFECTS

{-| Fetch emails and map this task to an effect that results in a GetEmails Action. -}
getEmails : Effects Action
getEmails = safeGet |> Task.map (\x -> GetEmails x.emails)
                    |> Effects.task

{-| Sleep for a certain amount of time and then perform the same functionality as the getEmails task. -}
checkEmails : Time -> Effects Action
checkEmails time = Task.sleep time
        `andThen` (\_ -> safeGet)
          |> Task.map (\x -> GetEmails x.emails)
          |> Effects.task

{-| Maps the get task to succeed whenever an error occurs with an empty list of emails. -}
safeGet : Task x Emails
safeGet = get `onError` (\err -> succeed {emails=[]})

{-| Task that performs a Get request to fetch emails at the specified URL. -}
get : Task Http.Error Emails
get = Http.get emails "https://api.myjson.com/bins/19lg3"

{-| A JSON decoder for the fetched object containing the emails -}
emails : Decoder Emails
emails = object1 Emails ("emails" := list Email.dataDecoder)

type alias Emails = { emails: List(Email.Data) }

-- INPUTS

{-| Helper method for creating a shortcut using the Alt key as the main key and mapping it to an action when pressed.
Note that this is the case in the other case also, but then the NoOp action is called which is actually doing nothing. -}
shortcut : Char -> Action -> Signal Action
shortcut key action = Shortcut.alt (\_ pressed -> if pressed then action else NoOp) key

{-| Helper method for creating a shortcut using the Alt key as the main key and mapping it to an action when released
and another one when pressed. -}
shortcut2 : Char -> Action -> Action -> Signal Action
shortcut2 key actionPressed actionReleased = Shortcut.alt (\_ pressed -> if pressed then actionPressed else actionReleased) key

{-| Merge all created shortcut to one Action signal -}
shortcuts : Signal Action
shortcuts =
  Signal.mergeMany [ shortcut 'J' FocusNext, shortcut 'K' FocusPrevious
                   , shortcut 'O' Truncate, shortcut 'P' Pin, shortcut 'X' Mark
                   , shortcut2 'S' (Sort DateReverse) (Sort PinDate)
                   , shortcut 'Q' HideDoneItems, shortcut 'L' HideAddReminder
                   , shortcut 'N' CollapseSnooze, shortcut 'M' Snooze ]

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  -- Split the ItemList in todo items and done items and sorting them by the current sorting strategy.
  -- Index the lists so that the focusIndex can take effect. The focusIndex runs over all items (todo+done)
  let todoList    = todo model |> sort (comparison model) |> List.indexedMap (\index el -> (index, el))
      doneList    = done model |> sort (comparison model) |> List.indexedMap (\index el -> (index+(List.length todoList), el))
  in Html.div []
  (List.concat [
    (if List.length todoList > 0 then
      [Html.div [] [
        Html.h2 [] [Html.text "To-do"],
        listView address model todoList]] else []),
    (if List.length doneList > 0 && not model.actions.hideDone.state then
      [Html.div [] [
        Html.h2 [] [Html.text "Done"],
        listView address model doneList]] else []),
      (if not model.actions.hideAdd.state then [Html.div [] [
        Html.h2 [] [Html.text "Reminder"],
        (ReminderForm.view (Signal.forwardTo address Form) model.form)]] else [])])



listView : Signal.Address Action -> Model -> List (Int, (Int, Item.Model)) -> Html
listView address model list = Html.div  [ A.attribute "class" "list-group" ]
                                  ( List.map (\(index, element) -> Html.div [ A.attribute "class" ("list-group-item" ++ (if index == model.focusIndex then " active" else ""))]
                                                             [(ItemList.elementView (Signal.forwardTo address Items) element)]) list)
