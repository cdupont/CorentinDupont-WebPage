---
title: Nomyx V1.0 will be out for Christmas!
description: ""
tags: 
---

...Or a liiiiittle bit after.
OK, maybe in the beginning of January.
[Nomyx](http://www.nomyx.net/) has undergone a long transition period, with lots of new features and changes but no official releases.
In the mean-time, you can already play with Nomyx by installing it [from github](https://github.com/cdupont/Nomyx) on your computer.

The main design goal for Nomyx V1.0 is to allow players with no knowledge of Haskell to play.
Nomyx V1.0 will contain:

- A new GUI
- A library of ready-made rules, that you can submit in one click
- The library also allows to make small modifications on the rule before submitting it
- An API and client application, allowing to play Nomyx from the command line. This allows you to develop your own rules in Haskell with your preferred tools (emacs, vim...) and sharing them in GIT.

Here are some previews of the GUI:

![Main page](/images/nomyx-main.png)

The main page allows to see the description of the game, and the list of players.

![The rules](/images/nomyx-rules.png)

The "Rules" page shows the rules that are currently active, pending and deleted.
The active rules are like the constitution of a country: all subsequent actions must comply with them!

![Input/outputs](/images/nomyx-ios.png)

The Input/Ouput panel shows a sum up of the IOs and possible interactions for each rules.

![The library](/images/nomyx-library.png)

The library proposes a selection of ready-made rules.
You can modify them and create new ones.
When you are ready, you can submit them in the main game.

Internally, the game has been entierely refactored.
I created a [new Complex Event Processing library](https://github.com/cdupont/Nomyx/tree/master/Imprevu) especially for Nomyx.
This CEP engine allows the rules to have effects (such as interacting with the players) at any time during the game.

I hope this version will be fun! 
