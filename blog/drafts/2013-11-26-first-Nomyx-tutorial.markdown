---
title: First Nomyx tutorial
description: a tutorial for the Nomyx newbie
tags: Haskell, Nomyx
---

Hi everyone!
To get a first understanding of Nomyx, you can watch the [introduction video](http://vimeo.com/58265498).
The home page of the game is [here](http://www.nomyx.net/).

To play Nomyx, connect on this [server](http://www.nomyx.net/).

To follow this tutorial, there are two options:
* install the game on your machine. Instructions are [here](http://www.nomyx.net/),
* play using the online [game](http://www.nomyx.net:8000/Nomyx).

If you don't have an Haskell environment already installed, the second solution if recommended.

So let's go!
After logging in, you will choose a screen name and register an e-mail.
Registering an e-mail is important: the game is quite slow-paced, so you will receive an email whenever a new rule is submitted by another player.
A game can last some weeks, usually the players spend 10 minutes per day to review the new rules, vote, and sometime propose new rules.

After this step, you will arrive at the main screen.

You can view any of the running games.
For this tutorial, we will create a new game: 

    click on "Fork".
    On the newly forked game, click on "Join".

Don't worry, this has no impact on the running games. You will be the only one to see the forked game: this will be our play ground.


#Your first rule

Let's begin with something simple!
We will propose a rule that does nothing. Doesn't seem to be much, eh?
Our rule that does nothing will anyway trigger the already active rules.

So, in the field "Enter a new rule", enter the following:

    Name: Do nothing
    Description: my first rule
    Code: nothing

In the big field "Code:", enter the word "nothing" (not capitalized).
"nothing" is a rule, defined in







