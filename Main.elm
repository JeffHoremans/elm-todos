module Main where

import StartApp
import Task exposing (Task)
import Effects exposing (Never)
import Html exposing ( Html )
import Signal

import Todos

-- Name: Jeff Horemans
-- Student ID: r0256304


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed
-- Summary: I've implemented a simple Shortcut module (Modules/Utils/Shortcut.elm). It allows easy creations of
--          shortcuts. It basically creates a Boolean signal, applying the and function on a number of keyboard signals.
--          It support a limiting amount of 2-key and 3-key shortcuts and maps them using a given mapping function.
--          I've done this for easy mapping to Actions in modules. For instance in the ItemFeed module I can easily and ellegantly
--          define shortcuts mapped to an action, albeit a helper function. It is also very readable and extensible, as new shortcuts
--          and different kind of shortcuts can be added very easily. Although the Shortcut module is far from perfect,
--          it contains quite alot of usefull methods, more than I used for this project. For more documentation see the module itself.
--
--          For this extension I used the combination Alt+Q which was easily added in the shortcuts method of the ItemFeed modules
--          (cfr. INPUTS section). The shortcuts method merges all shortcuts (Signal of ItemFeed Action). The Todos module defines
--          a shortcuts method as well, mapping and merging the shortcut signals of its nested modules (in this case, only the item feed, but as stated
--          in the method documentation, possibly others as well) to Todos Actions. This signal (of now a Todos Action) is passed to
--          the startapp module which ties the application together, updating the todos module with the nested item feed actions defined
--          by the shortcuts on keyboard combinations. When Alt+Q is pressed, the action HideDoneItems will eventually be called.
--          This actions maps to a nested action, namely of the ItemFeedActions module (see the module itself for more documentation).
--          The ItemFeedActions module contains a binary button that contains the state of the visibility of the 'done' items and is updated by the action.
--          In the view of the ItemFeedModule I do a simple check on this state and include the listView of the done items based on it.


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed
-- Summary: Cfr previous extension, used combination Alt+L.
--          Some additional remarks:
--          To hide the functionality by default I used the functionality of my BinaryButton module.
--          By simply initializing it with a True state, the add reminder form will be hidden initially.
--          For the rest, it works the same way as the hide done implementation
--          (the state also resides in a BinaryButton nested in the ItemFeedActions module
--          and the view of the ItemFeed module also check for this state to hide/show the form)


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed
-- Summary: I used the getCurrentTime method from the TaskTutorial package (http://package.elm-lang.org/packages/evancz/task-tutorial/1.0.3/)
--          which creates a task containing the current time (in milliseconds). Using effects I call the getTime task when initializing
--          The ReminderForm module (cfr the module itself for more documentation (modules/Todos/Feed/ReminderForm.elm)) which uses this method and maps it
--          to an Effects Action (Tick) containing the time. The time is then stored in the model and the form data is formatted using this time.
--          It then on his turn calls an task as an effect, called tick which does the same but sleeps for a while (a second), thereby every
--          second, updating the model with the current time.
--          I used the same principle in other modules where time was needed (Reminder, SnoozeButton, ...)


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed
-- Summary: The deadline property is optional, so I made the deadline property a Maybe String. Reminders added without it
--          either when added from the form with an empty deadline, or from the Static module which do not contain any,
--          are mapped witch a Nothing value for the deadline property. In the view of reminders, a simple case determines
--          to show deadline information or not.
--          To check if a deadline is past due, I've implemented a method called isPastDue in the Reminder module (modules/Todos/Feed/Items/Item/Reminder.elm).
--          This method checks if the deadline isBefore the current time (cfr getTime) using the isBefore method of the DateUtils module, which
--          which I implemented with a few handy methods for working with times and dates.
--          In the view, whenever the methods gives a positive result, the due date is marked red (text-danger).


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed
-- Summary: Firstly I implemented a snooze button module, which contains a button for toggling a small form
--          with a due date and contains the state of an item being snoozed or not. For more documentation, see the module (modules/utils/Button/SnoozeButton.elm).
--          An Item contains an ItemActions module (modules/todos/feed/items/ItemActions.elm) which contains such a SnoozedButton.
--          Whenever the snooze button is clicked (BinaryButton) the form is shown, when the ok button is clicked, the snooze State
--          of the SnoozeButton is set to True (the due date is already updated (setDue)).
--          The ItemFeed filters out snoozed items in its view so that snoozed items are not visible in the feed.
--          The focus methods and it helper methods do take this in account as such snoozed items are skipped.
--          Unsnoozing items whenever its due date is passed happens in the SnoozeButton module as well.
--          Its model is updated every second with the current time and checks if its state is set to True, if so if the due date is passed it calls the Unsnooze action.
--          The snooze state is set to False, the due set back to default and the item becomes visible again.
--          To test this, you can snooze to the date of today; the item should  become visible again within a second.
--          By default the next day is shown in the due date field using the plusDay method implemented in the previously stated DateUtils module.


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed
-- Summary: Implemented using effects and tasks much in the same way as the time retrieval. The initial effect of the item-feed
--          is the getEmails task which uses the elm-http library to fetch the json emails. Using a simple imlemented decoder the
--          fetched emails are decoded and mapped to the GetEmails action which maps the emails to items and adds them to the
--          item-list of the feed. Using the safeget method a bad result will result in the empty list.


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed
-- Summary: Much in the same way the getTime task calls the tick task, the getEmails task from the previous extensions calls
--          the checkEmails task which fetches every minute the emails from the url. The check in the ItemList modules (modules/todos/feed/items/ItemList.elm)
--          on duplicate Items (equal method in Item) prevents duplicate emails.


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed
-- Summary: Implemented using third-party elm-storage package: https://github.com/TheSeamau5/elm-storage/ announced on: http://elm-lang.org/blog/announce/0.15.
--          It wraps the localstorage operation which are not referentially transparant into tasks which again we use in the same way as before.
--          The initial effect of the Todos modules (modules/Todos/Todos.elm) is the getState task/method. It uses the elm-storage
--          library to get stored state of the application (if present --> Maybe). It is then mapped to an action which initializes
--          the application with the stored state if a state was stored, or initializes the default otherwise (the Static items).
--          Storing state happens whenever an update happens to the application. The Todos module consists of the ItemFeed module and the Settings module,
--          so in the update of Todos, when a nested action happens on the feed or the settings, the state should be stored. Therefore I added a State section in each module.
--          Each module has to provide a State type with all data needed to restore from state, a initState method to initialize a module from a state,
--          a state method to get the state from the current model, a decoder and encode method to decode and encode the state respectively and finally
--          a stateEqual method which I discuss later.
--          These methods provide for a recursive state of the application, as each module defines its state by either its values
--          or the state of its nested modules. Requesting the state of the item-feed will recursively requests the states of its
--          nested modules if need be, the same applies when encoding or decoding.
--          This implementation worked like a charm, albeit I noticed a lot of localstorage updates, this was caused by the time updates
--          which resulted in many many updates. And every update resulted in storing the state of the application to localstorage.
--          Too drastically reduce this number, I enforced the stateEqual method which recursively checks if the two states are equal.
--          In this method whenever a time value is compared I check only the string representation, thereby only when the day changes two states
--          are considered different from each other because of the time, and not every second. In the Todos update method, when the feed or
--          settings change because of a nested update. I call the stateEqual method between the updated and the original to determine a change.
--          Because this filtered out the time updates, now only whenever an action is performed (like pinning an item or changing something) a store operation happens.
--          This store operation is called in the effects by the storeState method similarly to before.


-- * Come up with your own extension!
-- * New type of item: Notification
-- Status: Completed
-- Summary: I've added a new type of item to show the extensibility of the Item module.
--          The Item module froms an interface for different types of items. To achieve this  it enforces a number
--          of methods and fields items should have. It also does not contain a regular init method, but constructors for each type of item.
--          It has thus a reminder, email an notification method that initializes a reminder, email and notification item respectively
--          based on the given item data. The Item module supports on the ItemType union type which defines the types of items and
--          wraps their Models. On each place where item type specific behaviour our data is needed a case is used on this ItemType.
--          For a new type of item to add, an additional branch for each case is thus required.
--          Item has a number of inspectors, some of them are defined for all kinds of items and do relay to the ItemActions Module
--          which contains all actions (with their state) that all items should have (like pinning, snoozing, marking as 'done').
--          Methods like date, icon and equal do need a case as mentioned before and call the appropriate action/method of the specific item module.
--          The Notification module itself is kept fairly simple and is added mainly for the purposes stated before.
--          Besides the changes to the Item module, the rest of the application above works
--          perfectly without knowing something has changed. Notifications are simply considered items as well and new types of Items
--          could be added quite comfortably.
--          I've added some notifications in the Static module ...

-- * Change the theme of the application!
-- Status: Completed
-- Summary: The previously mentionned settings module enables the user to dynamically change the theme (stylesheet) of the application.
--          The Todos module uses this theme value to dynamically change the value of the css file included in the link tag.
--          I've used some variations from http://bootswatch.com on bootstrap for this purposes.
--          Changing the theme works but has some flashing effect when the css is being changed, with transitions this can perhaps
--          be solved, but I did not work further on that.

-- * Shortcuts for collapse the snooze form and snoozing + next day by default
-- Status: Completed
-- Summary: Similarly like before: Alt+N for collapsing the snooze form and Alt+M for snoozing itself
--          For showing the next day by default in the snoozing form I used the plusDay method of the
--          my DateUtils module wich counts a day of milliseconds together with the current time.

-- * Clear storage
-- Status: Completed
-- Summary: Added a clear storage button to clear localstorage. Originally for testing purposes, but could be helpfull for you as well.


app: { html : Signal Html, model : Signal Todos.Model, tasks : Signal (Task Never ())}
app =
  StartApp.start
    { init = Todos.init
    , update = Todos.update
    , view = Todos.view
    , inputs = [Todos.shortcuts]
    }

main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
