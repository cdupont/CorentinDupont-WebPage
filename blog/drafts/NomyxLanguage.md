---
title: Crash course on the Nomyx Language
tags: Haskell, Nomyx
---

[TOC]

Welcome to the crash course on the Nomyx Language!
Don't forget to first watch the introductory video and GUI presentation at [www.nomyx.net](http://www.nomyx.net).
During this course we'll go through the examples in the file Examples.hs, that will allow us to introduce smoothly all the concepts of Nomyx.

The first rule is:

```haskell
nothing :: Rule
nothing = return ()
```

The type of this function is Rule, which is a synonym for `Nomex ()`: 

```haskell
type Rule = Nomex ()
```

A Nomex is an instance of Monad.
As its name hints, this function does nothing!
However if you submit it, it will trigger an event called `Rule Proposed`.
This event will wake up the handlers of the active rules that are subscribed on this event.
Thus, event if this rule `nothing` doesn't do anything, submitting it can trigger a lot of things! 
When you submit it, the votation rule (usually rule #1) is woken up and will display the forms for voting.
If the vote comes out as positive, rule #1 will activate and effectively run the incoming rule.

Only an existing active rule can activate a proposed rule: that's what makes the beauty of the game.
The logic for voting (democratic, monarchic...) is provided by the players and can be changed at any time.
A rule can also suppress and modify an existing rule.

# Ouputs

Here is our Hello World:

```haskell
helloWorld :: Rule
helloWorld = outputAll_ "hello, world!"
```

We use the output system of Nomyx, that you can find in the module Language.Nomyx.Outputs.
The output system is not merely a logging system but is dynamic: a given output can be automatically updated by the system at any time.
For example, you create an output that displays the current number of players:

```haskell
displayNbPlayers :: Rule
displayNbPlayers = void $ newOutput (Just 1) $ do
   pls <- getAllPlayerNumbers
   return $ "There are " ++ (show $ length pls) ++ " players"
```

If new players are coming or players are leaving, the message will update automatically.
You can create dynamic messages for pretty much anything. 
See for example how to display a bank account balance (function displayBankAccount), the current time (function displayTime), the status of a vote (in file Vote.hs), etc.

`newOutput` takes a player number to whom display the message (or Nothing for all players).
The second argument is a small program with the type `NomexNE String`.
This is a Nomyx expression that will return a string.
A NomexNE is different from a Nomex: "NE" stands for "No Effect". 
Indeed this small program passed in parameter of `newOuput` should not have any effect on the game state!
Otherwise very strange things could happen, such as increasing your bank account each time you press F5 (refresh screen on browsers).
`NomexNE` is a sub-DSL of `Nomex`, collecting all instructions that doesn't have any effect on the game state.
By comparison, `Nomex` collects all instructions that can have an effect.
Of course, a rule can have an effect on the game (but doesn't return anything), that's why its type is `Nomex ()`.


Other functions in Outputs.hs allow to manage the outputs: `getOutput`, `updateOuput` and `delOutput`.

#Variables

Let's create bank account to store our money!
This is the following rule:

```haskell
createBankAccount :: Rule
createBankAccount = void $ newVar_ "account" (0::Int)
```

We are creating a variable with the name "account", the type "Int" and the initial value 0.
All the variable related functions are located in the module Language.Nomyx.Variables.
You can see this variable appear in the section "Details" at the bottom of the game screen.
This variable can now be accessed by any other rules.
In the file example.hs you can see how to display this variable using dynamic outputs (`displayBankAccount`) and modify it (`winXEcuPerDay`, `winXEcuOnRuleAccepted`...).

As shown in Example.hs, it is useful to create a constant value to hold your variable name and type, so that each function acting on the variable can easily refer to it:

```haskell
accounts :: MsgVar [(PlayerNumber, Int)]
accounts = msgVar "Accounts"
```

A `MsgVar` is a refinement of a variable: it is a variable with a message attached.
Each time the variable will be updated or deleted, a `Message` event will be sent.
Other rules are able to subscribe on those events and will be waken up in case of any changes on the variable.

Now a rule can delete this variable easily:


```haskell
delAccounts :: Rule
delAccounts = delMsgVar accounts
```

#Inputs

A rule can create fields on the screen such as buttons, text boxes or radio buttons to collect inputs from the players.
In the file Inputs.hs you can find a lot of helpers to create such fields and their callbacks.
For example:

```haskell
enterHaiku :: Rule
enterHaiku = void $ onInputTextarea_ "Enter a haiku:" outputAll_ 1
```

This rule will display a text area on the screen of player 1, and prompt him to enter a Haiku.
Once he validates his entry, the poem will be displayed on the screen of each player.
The display is performed by the callback of onInputTextarea_:

```haskell
onInputTextarea_ :: String -> (String -> Nomex ()) -> PlayerNumber -> Nomex EventNumber
```

The callback takes the string entered by the player as an input, and can perform any effect on the game (thus the `Nomex` type). There are similar functions for creating text boxes, buttons, check boxes and radio buttons.

#Effects

Now that we overviewed some of the functions available in the rules, it's time to come back to their common point: They all have type `Nomex`.
`Nomex` is a DSL, defined in the file Expression.hs.
It is an instance of Monad, so the traditional "do" notation is available:

```haskell
displayActivateTime :: Nomex ()
displayActivateTime = do
   time <- liftEffect getCurrentTime
   outputAll_ $ "This rule was executed at: " ++ (show time)
```

This rule will display the time at which the rule has been activated and executed.
The type of getCurrentTime is not a `Nomex` but a `NomexNE`:

```haskell
getCurrentTime :: NomexNE UTCTime
```

We thus need to use the function `liftEffect` to use it in `displayActivateTime`:
     
```haskell
liftEffect :: NomexNE a -> Nomex a
```

We then use the function `outputAll_` to create a static display on the screen of all players:

```haskell
outputAll_ :: String -> Nomex ()
```

But, what if we wanted to display the current time instead of the activation time?
We need to use the dynamic version of the outputs, as explained above:

```haskell
outputAll :: NomexNE String -> Nomex OutputNumber
```

Our function becomes:

```haskell
displayCurrentTime :: Rule
displayCurrentTime = void $ outputAll $ do
   t <- getCurrentTime
   return $ "The current time is: " ++ (show t)
```

Here, instead of displaying a simple string, we are registering a small program that will yield the current time each time it is called.
The display system of Nomyx will then call this function each time you refresh the screen, thus always displaying the current time.


# Rules
 
Wow! With all that we learned so far, we are now able to construct more interresting rules.
Let's now change our political system: it's the revolution!
We want to go from the current democraty to a monarchy.
The democratie correspond to the initial voting system: everybody votes.
But in a monarchy, only the king (hopefully you) decides!
This is demonstrated in the rule `revolution`:

```haskell
revolution :: PlayerNumber -> Rule
revolution pn = do
   suppressRule 1
   rNum <- addRule' "Monarchy" (monarchy pn) ("monarchy " ++ (show pn)) "Monarchy: only the king can vote on new rules"
      activateRule_ rNum
      autoDelete
```

`revolution` is a sort of management rule: it will be executed once (if accepted), change some things, and then be deleted.
Indeed, once executed, it doesn't need to stay in the active rules: it will not do anything more.
That's why is has the instruction `autodelete`.
So, this rule first suppresses the rule number 1, which is usually the democratie.
Then, `revolution` adds directly another rule in the pending rules, using `addRuleParams`.
We pass the title of the added rule, the code (using both a function code and a string to display it) and a description.
Finally, we activate this new rule and delete ourself.

You are now the King! Only you will be prompted for any incoming rules. Be wise...
The kind of rules such as `revolution` is really what makes a Nomic game: the ability for a rule to manipulate other rules, including itself.

As a reminder, its always another already active rule that can activate a proposed rule.
This is done with the function:

```haskell
activateRule :: RuleNumber -> Nomex Bool
```

When called with a rule number, this function will change the status of the function from "Pending" to "Active", as can be seen on the top of the game screen.
Furthermore, it will execute the rule.
So, to go from democracy to monarchy, you have to repect the democracy rules that are active one last time, and then you can change the political system.

# Events

In the section on Inputs, we showed how to create single form fields such as a text field, and how attach a callback to them.
But what if I want several input fields combined together?
What if I want to combine the form results with other events happening in the game, such as a player arriving or a specific time is reached?
This is done through the event system of Nomyx. 
The inputs forms (text box, text area, button, checkboxes and radios) are in fact created by standard events in Nomyx.
Here is the complete list of events (in file Events.hs):

- an input field is completed,
- a player arrives or leaves,
- a rule is proposed, activated, rejected, added, modified or deleted,
- a specific time is reached,
- a message is sent by another rule,
- the victory condition is updated.

An event yielding a value `a` is represented by the type `Event a`.
Event is an instance of the following typeclasses: Functor, Applicative, Alternative, Monad, MonadPlus and finally Shortcutable.
This allows to combine the events in many ways, in an FRP style.

##Applicative interface

For example, if you need to fill a record with two fields containing the name and surname of your player, you can do everything in one go:

```haskell
data NameSurname = NameSurname {name :: String, surname :: String} deriving (Show, Typeable)

eventNameSurname :: Event NameSurname
eventNameSurname = NameSurname <$> (inputText 1 "Name:") <*> inputText 1 "Surname:"
```

Using the Applicative interface, you can fill up your record `NameSurname` with the result of the two text fields easily.
When submitting this event, two input fields will appear on the screen of player number 1, and their results will be combined into the NameSurname record.
You can register your event with the event system of Nomyx this way:

```haskell
displayNameSurname :: Nomex ()
displayNameSurname = do
   let displayNameSurname ns = void $ newOutput_ Nothing ("Name: " ++ (name ns) ++ " Surname: " ++ (surname ns))
   void $ onEvent_ eventNameSurname displayNameSurname
```

Once the event registered by your rule, the event list will be scanned by the Nomyx system.
If some inputs are found in the active event list, they will be automatically displayed to the players, in the "Inputs/Outputs" zone of the screen.
In Nomyx we are merging in the same representation two different activities that GUIs are traditionaly performing separately:

- displaying/positioning/styling some forms,
- attaching the logic to the result of the forms. 
Here everything is performed in the same language.

The players doesn't control where the forms are displayed and how they are styled: this is performed by the engine and GUI of Nomyx.
The objective of this DSL is to let the players concentrate on the logic of the rules.
Indeed the rules have to be read by other players and voted on, so they should be kept simple.

##Alternative interface

Now, what if, instead of a "product" datatype, such the NameSurname record, you need a "sum" datatype, such as a Boolean?
This is performed with the Alternative interface:

```haskell
eventBool :: Event Boolean
eventBool = True <$ inputButton 1 "click here for True" <|> False <$ inputButton 1 "click here for False"
```

This event creates two buttons, both on the screen of player 1 (but they could also appear on the screen of different players).
The first button clicked makes the whole event returns True, the second False.
Here is the type of `inputButton`:

```haskell
inputButton :: PlayerNumber -> String -> Event ()
```

We use the Functor operator `<$`, which is short for `const True <$>`. 
Finally, the whole expression `True <$ inputButton 1 "click here for True"` is typed `Event Bool`.
Using the `<|>` operator, we introduce a choice between the two `Event Bool`: the first that fires wins.

With both Applicative and Alternative, we can create any ADT from complex forms.
The forms can be distributed to several players.
This is specially useful for voting systems, where each player has to complete a vote form, and all the results are collected to form a single "vote" event.

##Monad interface

`Event` is also an instance of Monad.
We can create events that react on the result of previous events:

```haskell
eventName :: Event String
eventName = do
   myBool <- eventBool
   if myBool then inputText 1 "Enter your Name:"
   else return "No name"
```

This event create the two buttons of eventBool above.
If the "True" button is pressed, another input text field is created asking for a name.
Otherwise, the string "No name" is directly returned.

Other types of events can be combined.
For example, a timer can be embedded:

```haskell
eventQuickName :: UTCTime -> Event String
eventQuickName timeOut = inputText 1 "Your name, quick!" <|> "Too late!" <$ timeEvent timeOut
```

This event will ask you for you name, but after a certain time the text field will dissapear and a string "Too late" will be returned instead.

An `Event` can also access the content of the game state, and operate decisions on it.
This is performed by the function `liftNomexNE`, as showed in the example function `moneyTransfer`.

```haskell
moneyTransfer :: Rule
moneyTransfer = do
   let askAmount :: PlayerNumber -> Event (PlayerNumber, Int)
      askAmount src = do
         pls <- liftNomexNE getAllPlayerNumbers
         guard (length pls >= 2) >> do
            dst <- inputRadio' src "Transfer money to player: " (delete src $ sort pls)
            amount <- inputText src ("Select Amount to transfert to player " ++ show dst ++ ": ")
            return (dst, readDef 0 amount)
   void $ forEachPlayer_ (\pn -> void $ onEvent_ (askAmount pn) (transfer pn))
```

In this rule we create an Event `askAmount`.
`askAmount` will test if the number of player is more than 2.
If that's the case, it will display successively two fields, one for asking to which player you would like to transfer funds to, and the second for the amount.
Finally this event `askAmount` is registered for each player, with the appropriate callback `transfer`, so that everybody can transfer money to whom they want.


The objective of all that is to let the players create complex events while keeping a simple language.
This simplified version of Functional Reactive Programming allow us to do exactly that.
Without FRP (so Event not being an instance of anything), you would be obliged to attach a callback to each single event, and combine the results yourself, asynchronously: this would be very tedious for big events.


# Victory

The second most important feature of a Nomic game is to be able to change the way how to win the game.
This is performed by this instruction:

```haskell
setVictory :: NomexNE [PlayerNumber] -> Nomex ()
```

`setVictory` allows to set the victory condition.
Similarly to the output system, the victory is dynamic: it's a small program with type `NomexNE [PlayerNumber]` that returns the list of victorious people.
As soon as this NomexNE returns a non-null list, the victory will be annouced on the GUI to all players.
The rule `victoryXEcu` in the example file show how to change this condition: you will win if you have more than a given amount of ecus.
As soon as your bank account reaches that number: bingo!

You could also be bold and propose directly your own victory:

```haskell
iWin :: Rule
iWin = liftEffect getProposerNumber >>= giveVictory
```

This function takes the player number of the proposer of the rule (so, you) and sets the victory with it.
Good luck to have this rule accepted by other players!!


