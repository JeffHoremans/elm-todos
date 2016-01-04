module Item
    ( Model, State, Action(Truncate, Mark, Pin, Snooze, Unsnooze, CollapseSnooze)
    , initState, reminder, email, notification
    , isDone, isPinned, isSnoozed
    , state, equalState
    , encode, decoder
    , date, equal
    , update, view
    ) where

{-| Module representing an abstract item. -}
import Html exposing ( Html )
import Html.Attributes as A
import Signal
import Effects exposing ( Effects, none )
import Json.Decode as Decode exposing ( Decoder, (:=), object2)
import Json.Encode as Encode exposing ( Value )

import ItemActions
import Reminder
import Email
import Notification


-- MODEL

-- the type of the item containing the model of the specific item
type ItemType = ReminderItem Reminder.Model | EmailItem Email.Model | NotificationItem Notification.Model

-- consists of the itemType containing item type specific data and actions,
-- and the ItemActions module containing general item actions
type alias Model = {  item: ItemType,
                      actions: ItemActions.Model }

--- Item Constructors
-- No init method, but a constructor for each kind of item

{-| Initialize a reminder item. -}
reminder : Reminder.Data -> (Model, Effects Action)
reminder item =
  let (reminder, reminderFx) = Reminder.init item
      (actions, actionsFx) = ItemActions.init
  in (Model (ReminderItem reminder) actions, Effects.batch [Effects.map ReminderAction reminderFx, Effects.map ItemAction actionsFx])

{-| Initialize an email item. -}
email : Email.Data -> (Model, Effects Action)
email item =
  let (email, emailFx) = Email.init item
      (actions, actionsFx) = ItemActions.init
  in (Model (EmailItem email) actions, Effects.batch [Effects.map EmailAction emailFx, Effects.map ItemAction actionsFx])

{-| Initialize a notification item. -}
notification : Notification.Data -> (Model, Effects Action)
notification item =
  let (notification, notificationFx) = Notification.init item
      (actions, actionsFx) = ItemActions.init
  in (Model (NotificationItem notification) actions, Effects.batch [Effects.map NotificationAction notificationFx, Effects.map ItemAction actionsFx])

{-| Initialize an item from state. -}
initState : State -> (Model, Effects Action)
initState state = case state.item of
  ReminderState s ->      let (reminder, reminderFx) = Reminder.initState s
                              (actions, actionsFx) = ItemActions.initState state.actions
                        in (Model (ReminderItem reminder) actions, Effects.batch [Effects.map ReminderAction reminderFx, Effects.map ItemAction actionsFx])
  EmailState s ->         let (email, emailFx) = Email.initState s
                              (actions, actionsFx) = ItemActions.initState state.actions
                        in (Model (EmailItem email) actions, Effects.batch [Effects.map EmailAction emailFx, Effects.map ItemAction actionsFx])
  NotificationState s ->  let (notification, notificationFx) = Notification.initState s
                              (actions, actionsFx) = ItemActions.initState state.actions
                        in (Model (NotificationItem notification) actions, Effects.batch [Effects.map NotificationAction notificationFx, Effects.map ItemAction actionsFx])

-- Item inspectors

{-| Check whether an item is marked 'done' or not.
This state resides in the ItemActions module, so we retrieve it. -}
isDone : Model -> Bool
isDone model = model.actions.mark.state

{-| Check whether an item is pinned or not.
This state resides in the ItemActions module, so we retrieve it. -}
isPinned : Model -> Bool
isPinned model = model.actions.pin.state

{-| Check if an item is snoozed or not.
This state resides in the ItemActions module, so we retrieve it. -}
isSnoozed : Model -> Bool
isSnoozed model = model.actions.snooze.state

{-| Retrieve the date of an item. Maps to the corresponding fields of the nested ItemType Model -}
date : Model -> String
date model = case model.item of
    ReminderItem m -> m.data.created
    EmailItem m -> m.data.date
    NotificationItem m -> m.data.date

{-| Retrieve the iconClass of an item. Maps to the corresponding fields of the nested ItemType Model -}
icon : Model -> String
icon model = case model.item of
    ReminderItem m -> m.icon
    EmailItem m -> m.icon
    NotificationItem m -> m.icon

{-| Check if two items are equal. If they are of the same type, there data needs to be equal.
Otherwise they are considered different. -}
equal : Model -> Model -> Bool
equal a b = case a.item of
    ReminderItem ma -> case b.item of
        ReminderItem mb -> ma.data == mb.data
        _ -> False
    EmailItem ma -> case b.item of
        EmailItem mb -> ma.data == mb.data
        _ -> False
    NotificationItem ma -> case b.item of
        NotificationItem mb -> ma.data == mb.data
        _ -> False

-- STATE

-- union type for the type of itemstate containing the state of a specific kind of item
type ItemState = ReminderState Reminder.State | EmailState Email.State | NotificationState Notification.State

-- storing state requires the itemstate with its specific item state and the state of the itemActions
type alias State = { item: ItemState, actions: ItemActions.State }

{-| Map the given model to its state representation. Maps to the correct state
method of a specif item module based on its item type. -}
state : Model -> State
state model = case model.item of
  ReminderItem m -> State (ReminderState (Reminder.state m)) (ItemActions.state model.actions)
  EmailItem m -> State (EmailState (Email.state m)) (ItemActions.state model.actions)
  NotificationItem m -> State (NotificationState (Notification.state m)) (ItemActions.state model.actions)

{-| Check if two models have the same state representation. Similar to equal. -}
equalState : Model -> Model -> Bool
equalState a b = case a.item of
  ReminderItem ma -> case b.item of
    ReminderItem mb -> Reminder.equalState ma mb && ItemActions.equalState a.actions b.actions
    _ -> False
  EmailItem ma -> case b.item of
    EmailItem mb -> Email.equalState ma mb && ItemActions.equalState a.actions b.actions
    _ -> False
  NotificationItem ma -> case b.item of
    NotificationItem mb -> Notification.equalState ma mb && ItemActions.equalState a.actions b.actions
    _ -> False

{-| A JSON decoder for the state. To decode the item(type) we need to try them all as we
can at this point not know for certain which type of item has been stored.
We therefore try them all with the oneOf method, resulting in one of them succeeding. -}
decoder : Decoder State
decoder = object2 State
  ("item"       := Decode.oneOf [reminderDecoder, emailDecoder, notificationDecoder])
  ("actions"    := ItemActions.decoder)

{-| A custom JSON decoder for the reminder Itemstate -}
reminderDecoder : Decoder ItemState
reminderDecoder = Decode.customDecoder Reminder.decoder (\x -> Result.Ok (ReminderState x))

{-| A custom JSON decoder for the email Itemstate -}
emailDecoder : Decoder ItemState
emailDecoder = Decode.customDecoder Email.decoder (\x -> Result.Ok (EmailState x))

{-| A custom JSON decoder for the notification Itemstate -}
notificationDecoder : Decoder ItemState
notificationDecoder = Decode.customDecoder Notification.decoder (\x -> Result.Ok (NotificationState x))

{-| JSON-encode the given state. Depending on the ItemType/StateType the correct
encode method is used of the specific item module. -}
encode : State -> Value
encode state = Encode.object
  [ case state.item of
      ReminderState s ->     ("item", Reminder.encode s)
      EmailState s ->        ("item", Email.encode s)
      NotificationState s -> ("item", Notification.encode s)
  , ("actions",   ItemActions.encode state.actions)]

-- UPDATE

type Action = -- Nested ItemActions action
                 ItemAction ItemActions.Action |
              -- Nested specific item actions
                 ReminderAction Reminder.Action | EmailAction Email.Action | NotificationAction Notification.Action |

              -- Exposed actions that map to nested ItemActions
                 Mark | Pin | Snooze | Unsnooze | CollapseSnooze |
              -- Exposed actions that map to specific item Actions
                 Truncate

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    -- Exposed actions that are mapped to nested ItemActions actions
    Pin             -> update (ItemAction ItemActions.Pin) model
    Mark            -> update (ItemAction ItemActions.Mark) model
    Snooze          -> update (ItemAction ItemActions.Snooze) model
    Unsnooze        -> update (ItemAction ItemActions.Unsnooze) model
    CollapseSnooze  -> update (ItemAction ItemActions.CollapseSnooze) model

    -- Exposed actions that are mapped to nested specific actions, in this case only the
    -- Truncate action is supported which calls the Collapse action of the Email module.
    -- But other potential item specific actions could be added easily.
    Truncate        -> update (EmailAction Email.Collapse) model

    -- Internal nested ItemActions actions update the ItemActions model
    ItemAction subAction -> let (actions, fx) = ItemActions.update subAction model.actions in ({ model | actions <- actions}, Effects.map ItemAction fx)

    -- In all other cases, its needs to be a Item specific action (either ReminderAction, EmailAction, ...)
    -- We case over them and update the specific item appropriately.
    _ ->
      case model.item of
        ReminderItem r -> let (reminder,fx) = (updateReminder action r)
          in ({model | item <- ReminderItem reminder}, Effects.map ReminderAction fx)
        EmailItem em -> let (email,fx) = (updateEmail action em)
          in ({model | item <- EmailItem email}, Effects.map EmailAction fx)
        NotificationItem n -> let (notification,fx) = (updateNotification action n)
          in ({model | item <- NotificationItem notification}, Effects.map NotificationAction fx)

{-| Update a reminder item with the given action. -}
updateReminder: Action -> Reminder.Model -> (Reminder.Model, Effects Reminder.Action)
updateReminder action model = case action of
  ReminderAction subAction -> Reminder.update subAction model
  _ -> (model, none)

{-| Update an email item with the given action. -}
updateEmail: Action -> Email.Model -> (Email.Model, Effects Email.Action)
updateEmail action model = case action of
  EmailAction subAction -> Email.update subAction model
  _ -> (model, none)
{-| Update a notification item with the given action. -}
updateNotification: Action -> Notification.Model -> (Notification.Model, Effects Notification.Action)
updateNotification action model = case action of
  NotificationAction subAction -> Notification.update subAction model
  _ -> (model, none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let itemView = case model.item of
    -- case over itemType to get the appropriate item view which renders item specific data.
    -- Data that all items should have are extracted using inspectors like date and icon.
    ReminderItem m -> Reminder.view (Signal.forwardTo address ReminderAction) m
    EmailItem m-> Email.view (Signal.forwardTo address EmailAction) m
    NotificationItem m -> Notification.view (Signal.forwardTo address NotificationAction) m
  in Html.div [A.attribute "class" "row"] [
      Html.div [A.attribute "class" "col-lg-1"] [
        Html.h1 [A.attribute "style" "margin-top: 0;"] [Html.span [A.attribute "class" ("glyphicon glyphicon-" ++ icon model)] []]
      ],
      Html.div [A.attribute "class" "col-lg-10"]
        (List.append (itemView :: ItemActions.view (Signal.forwardTo address ItemAction) model.actions)
          [Html.br [] [], Html.p [A.attribute "class" "pull-right"] [Html.text (date model)]]),
      Html.div [A.attribute "class" "col-lg-1"] [
        Html.h1 [A.attribute "style" "margin-top: 0;"] (if model.actions.pin.state then [Html.span [A.attribute "class" "glyphicon glyphicon-pushpin", A.attribute "style" ""] []] else [])
      ]]
