---
title: Do we live in a simulation?
description: The Sims
tags: Physics
---

As I hinted in [this post](http://www.corentindupont.info/blog/posts/Cosmology/2014-07-21-Math-Structure.html), our world might be mathematical.

We perceive our world as physical: I can touch and see the table I am writing this article on.
But at a lower level, there is only interaction between particles.
I perceive my world as physical because I live inside it!

If it was ever possible to see the Universe from outside of it, I wouldn't see directly galaxies and planets.
I would see it only as a Math struture.
Galaxies, planets, Humans and flowers are deeply embbed sub-structures inside it.


Not the Sims
------------

It is very probable that our perceived world is a simulation.
But not a simulation running in a PC, with a screen and an alien in front of it!
A much lower-level simulation.

Take the Conway game of life.
This is a very simple cellular automata, running only 3 basic rules.
Despite it's extreme simplicity, the game of life is Turing complete!
Than means that we are able to build a fully functionning computer inside a game of life.
And with a computer, we can build a simulation.

The idea is that we could live inside this sort of simulation: a simulation running on very simple laws, simulating the more complex laws of our world.
This simulation could produce all the laws of physics.
It could generate the space, and maybe also the time that we know.

The idea that we live in a simulation looks complex at first sight (not very compliant with Occam's razor).
But on the contrary it is simplifying: it allows to explain the complex laws of our universe with simpler laws.


![Turing](http://rendell-attic.org/gol/turing_js_r.gif)

![Conway](/images/conway.jpg)


The simulation game
-------------------

So how do we prove that we live in a simulation?
From a computer science perspective, the equivalent question would be:
How a software can know if it is running in a Virtual Machine (rather than on bare metal)?


Game #1

This game will provide a very simple Univers simulator.
The simulator is a black box.
Each player will insert in the simulator a small program (a citizen).
The goal for the citizen is to discover the laws of the Univers it lives in.

For example, the Universe will provide this entry points:
- bool doSomething()

Our Citizen is only able to... Do something.
He has a sense that sometimes sends him a signal, sometime not.
He is totally blind.

How would you go to probe the laws of that universe?
Well, behind the hood, our creature has a position.
Each *doSomehing* will actually move him in a random direction.
His only sense is able to detect that he was already in the same position.
Would you be able to detect how many dimensions this Universe has?

Game #2

Ok, if you don't find it fun enough, here's another game.
The problem with this first game is:
- the API gives us a lot of clues
- the citizen are programmed in an existing, external programming language: the Univers is not really providing this language.

In the case of our Universe, the main difficulty is that we live inside.
We need to discover the laws of the Universe, using those laws in the process.
That's what makes it non trivial!

So, imagine that you are a program, living inside a computer.
How would you go to find out in which programming language you were written?
Of course, the *programer* knows.
But the program itself?
This supposes that the program has the capacity to represent the primitives and mechanims of his how base language.
This is the case of any Turing complete language: A Turing complete language is able to emulate any other Turing complete, including itself.  

This is the same for us: nothing guaranties that the base language of the Universe is powerful enough to be able to contain a description of itself.
If it's not the case, nothing will allow us to access those base rules, even for the most intelligent person on Earth...


https://en.wikipedia.org/wiki/Church%E2%80%93Turing%E2%80%93Deutsch_principle
https://www.mat.univie.ac.at/~neum/physfaq/topics/touch.html
