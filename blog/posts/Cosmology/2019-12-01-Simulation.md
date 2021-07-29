---
title: Do we live in a simulation?
description: The Sims
tags: Physics
---

As I hinted in [this post](/blog/posts/Cosmology/2014-07-21-Math-Structure.html), our world might be mathematical.

We perceive our world as physical: I can touch and see the table I am writing this article on.
But at a lower level, there is only interaction between particles.
The key point is, I perceive my world as physical only because I live inside it!

If it was ever possible to see the Universe from outside of it, I probably wouldn't see galaxies and planets.
All I would see is an enormous Mathematical structure.
Galaxies, planets, Humans and flowers are deeply embbeded sub-structures inside it.


Not the Sims
------------

It is very probable that our perceived world is a simulation.
But not a simulation running in a PC, with a screen and an alien in front of it!
A much lower-level simulation.


![Our Universe in some alien's computer](/images/alien.png)


Take the [Conway Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).
This is a very simple cellular automata, running only 3 basic rules.
Despite it's extreme simplicity, the game of life is Turing complete!
Than means that we are able to build a fully functionning computer inside a Game of Life.
And with a computer, we can simulate any process.

![A Turing machine built inside Conway's game of life ](http://rendell-attic.org/gol/turing_js_r.gif)

The idea is that we could live inside this sort of simulation: a simulation running on very simple laws, simulating the more complex laws of our world.
This simulation could produce all the laws of physics.
It could generate the space, and maybe also the time that we know.

The idea that we live in a simulation looks complex at first sight (not very compliant with Occam's razor).
But on the contrary it is simplification: it allows to explain the complex laws of our universe with simpler laws.
In this kind of simulation, complex structures could appear, and why not even life.

![Life in Conway's game?](/images/conway.jpg)

So, imagine that you were a living entity inside a huge and long lasting Game of Life simulation.
How would you perceive the world?
And how would you know that you live in a Game of Life?
From your point of view, the world might have 3 dimensions.
It doesn't matter that the reality behind your Universe, i.e. Game of Life, have only two dimensions: remember that the world you live in is simulated.
It might be composed of particules like electrons and photons...
From your point of view, your world is real.
When you kick a rock, the rock moves and your foot hurts.

From the "outside" of the Game of Life, your existence might be very difficult to perceive.
The cell blocks involved into your existence might be disseminated over a huge part of the Game's space.
All an external observer would see is a very, very big Game of Life going on.


The simulation game
-------------------

So how do we detect that we live in a simulation?
An analogy comes to mind with Virtual Machines used in many places for computing.
A Virtual Machine is a piece of software running on a computer.
It simulates the properties of another type of computer (for example, a Windows VM on a Linux machine).
So the question is: How a software can know if it is running in a Virtual Machine (rather than on bare metal)?
Let's play a game!

In this game, you need to imagine that you are an "intelligent" program, that we will call a Citizen.
This Citizen lives in a computer simulation, that we will call the Universe.
The goal for the Citizen is to discover the laws of the Universe it lives in.

The game will provide a very simple Universe simulator.
Each player will insert in the simulator a Citizen (i.e. a simple program).
As a first example, the Universe will provide these two entry points to the Citizens:
```
doSomething()
bool senseEnvironment()
```

Our Citizen will only able to... Do something.
He also has a single sense, with a boolean result.
He is otherwise completely blind.
How would you go to probe the laws of that Universe?
In each round of this game, a different Universe is proposed.
Up to you to propose a program that will probe the Universe and allow you to make the right deductions.

For example, the first Universe will feature just two hidden states: A and B.
Calling `doSomething()` will just change your state from A to B and B to A. Calling `senseEnvironment` will return `True` if A is sensed, `False` if B is sensed.
After a series of trials, it should be easy for you to deduce the law of this Universe: a simple flip with two states.

In the second Universe, our Citizen is given a position.
Each call to `doSomething()` will actually move him one step in a random direction.
His only sense is able to detect that he was already in the same position at some point in the past.
You can imagine a snail, or an ant, that leaves olfactive traces.
Will you be able to deduce the structure of that Universe?
In particular, would you be able to detect how many dimensions this Universe has?
Furthermore, if this Universe was created inside a Conway Game of Life, will your Citizen be able to detect it?
At this stage, the answer is clearly "no".
Our simple sensor with two states does not allow to probe that far.
The real nature of the Universe is definitely lost to our Citizen.

The open question is, does there exist a Universe, more complex than the one that we just described, that would allow the Citizens to peek into its base rules (i.e. the Game of Life)?
In the next article, we'll try to see and complexify our sample Universe and Citizen.



https://en.wikipedia.org/wiki/Church%E2%80%93Turing%E2%80%93Deutsch_principle
https://www.mat.univie.ac.at/~neum/physfaq/topics/touch.html
