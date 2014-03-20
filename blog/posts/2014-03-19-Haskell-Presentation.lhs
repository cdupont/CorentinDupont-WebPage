---
title: Haskell presentation
description: the slides from my Haskell presentation
tags: Haskell
---

[Here](/docs/intro_haskell.pdf) are the slides from my presentation of Haskell in the [Hackerspace Trento](https://sites.google.com/site/hackerspacetrento/). I tried to present some of the core concepts of Haskell, to an audience that have mostly experience in imperative programming. I show notably the key aspects of Haskell: functional, pure, lazy and statically typed, and I try to transmit my enthusiasm for the language!

Toward the end of the presentation, I chose to give an understanding of *IO* in Haskell without talking about *monads*: monads are certainly too advanced for a first contact with Haskell.
Especially because a monad, as short as the definition is, uses 4 new concepts to beginners: type classes, type parameters, higher order functions and sugar notation.
Only the higher order functions were covered in this lecture.

Instead I used the metaphor of "passing the World around" in each IO-involved functions (see slides 18).
This allows to show that pure functions, while not being able to "perform" IO at the moment they are run, can "program" IO by putting instructions in the "World" data structure that they output.
That makes these IO functions chainable.

How can we make an impure program (a pure program, effect less, would not be very useful), using a pure language?
When compiling an executable, your compiled code is put together with what is called a "runtime system" (see slide 19).
The runtime system takes care of executing the IO instructions that are found in the "World" data structure.
This run time system is certainly not pure.
However, it will call your pure program through the main, and react to IO programmed by IO functions that are passed through the World structure.
The key point to understand is that you can create an impure *program* by putting together a runtime system calling *pure* language instructions.

I went on saying that passing the whole state of the world would be quite heavy indeed, so we use a special construct called a monad.
This construct allows us to hide the heavy carrying of the World between functions.
Monads come with a syntactic sugar called do notation that allows us to write those neat imperative style IOs.
Hope that was understandable :)
