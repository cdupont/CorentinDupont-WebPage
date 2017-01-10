---
title: Applicative-Do in reactive GUIs (examples from Nomyx)
description: 
tags: Programming 
---

The new ApplicativeDo interface allows a more natural style in reactive GUIs.
We'll look at examples from Nomyx, which supports a reactive programming style for events.
This allows to create small GUIs on the fly.
With the new ApplicativeDo interface, it gets even better!

Nomyx creates small forms on the fly when some data is needed from the players.
For example this rule will display a text area on the screen of player 1, and prompt him to enter a Haiku.
Once he validates his entry, the poem will be displayed on the screen of each player.

    enterHaiku :: Rule
    enterHaiku = void $ onInputTextarea_ "Enter a haiku:" outputAll_ 1

Forms can be composed using an applicative interface:

    eventNameSurname :: Event NameSurname
    eventNameSurname = NameSurname <$> (inputText 1 "Name:") <*> inputText 1 "Surname:"

    displayNameSurname :: Rule
    displayNameSurname = do
      let displayNameSurname ns = void $ newOutput_ Nothing ("Name: " ++ (name ns) ++ " Surname: " ++ (surname ns))
      void $ onEvent_ eventNameSurname displayNameSurname






