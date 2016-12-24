---
title: Haskell is hard. Aaaaaaaaaaahhhhhhhhh!!!!! 
description: "" 
tags:
---

Programming in Haskell is not easy!
I sometime have my moments of doubt, sitting back and asking myself "why, but why do I use Haskell?".
There surely be some serious reasons!
Haskell have some serious qualities, no doubt, but they come with a price.

So, I gather here some advices that I gained along the way.
I include in this article mostly "don't" in Haskell.
For more advices on how to structure an Haskell program, check out the [second part](2016-12-14-Haskell-structure.html) of this article.

Don't use type level programming
--------------------------------

Type level programming in Haskell is fun, but don't use it in real programs.
Seriously, don't.
It might give your program a little more security or robustness.
However the cost of type level programming, in term of understandability, maintenance and evolution is too high.

As an example I spent a lot of time developing [a system](2014-01-30-DSL-Effect.html) that allows to isolate effectful from non effectful IO operations (i.e. separating the I for the O).
For example, and effectful operation is writing in a file or on the screen.
An effectless operation is to read from a file or from a MVar.
Effectless operations can be repeated as much as we can, without damage.
Separating effects can lead to a little more security for the programmer/user of your library.

I made this development for the [Nomyx Language](https://hackage.haskell.org/package/Nomyx-Language).
However this came with an overhead both for the maintainer (me) and the user of Nomyx.
So I recently decided to remove those tricks from the language.

There are [some propositions](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) to distinguish effects from non effects in Haskell IO.
However the cost in term of programming becomes too high in my opinion.
Standard Haskell separates pure operations from IO operations.
This has enabled a lot of nice features and programming methodologies.
Those features outweights the inconvenients.
However further separating IO operations yields a lower payoff in my opinion.


Don't stack your Monads (too much)
----------------------------------

To express the various operations of the program, one could use:

- a StateT for the variable part of the data (the program state),
- a ReaderT for the inmutable part (the program settings),
- a WriterT for the logging,
- a ExceptT for handling custom exceptions.

```
myBigFunction :: StateT Game (ReaderT Settings (WriterT String (Except String))) ()
myBigFunction = do
   game <- get
   set <- ask
   tell "log this"
   throwError "error"
   return ()
```
However, all this will be very painful to stack and unstack.
Using a huge monad stack can become very cumbersome, and not readable.
Instead, it's better to put everything in one State monad.
The `ReaderT settings` and the `WriterT String` parts can both be integrated in the StateT data structure.
It's a little less secure (for example immutable parts can still be modified in the StateT).
But it will be much more understandable on the long term.


Before making it beautiful, make it work
----------------------------------------

Haskell proponents often have a mind for beautiful code.
But it's often very difficult to make things perfect on the first try.
So I usually make a quick and dirty prototype first, and then refactor, refactor, refactor... Until I am satisfied.
I call this phase "straightening the strings"...
It feels very much like a fisherman untangling a net, and making direct, clear connections between components.
Sometime you don't need to understand all the details of the connections, you just need to shake the net and everything will fall in place.


If it ain't broke, don't fix it
-------------------------------

This one is even harder for Haskellers.
How much time do we loose, trying to make only slight improvements to our code?
I must say that refactoring to make a program more beautiful is also part of the fun with Haskell...
However, let's try to be reasonable and not refactor this old and ugly (but working) code... Arghhh!!


Don't make your own recursion
-----------------------------

Self-made recursion is hard.
Most of the time the recursion can be avoided by using one of the Prelude's functions: `map`, `foldr`, `mapM`, `sequence`...
If you need to make your own recursion, there is probably a design bug.

Don't use too much indirections
-------------------------------

Cut the middle man.
Having too much levels of indirections can be confusing.
One case where you need to rely a lot on higher order parameters is for inversion of control.
If you have two modules A and B, and A calls B, then B cannot call A: that would be an import cycle.
So you pass a function as parameter (a callback).
However refactoring a bit the program often allows for a more direct style of programming.



Bonus: my most hated error messages
===================================

Here are my most dreaded error messages:

```
Couldn't match type 'v' with ...
  'v' is a rigid type variable bound by...
```
This happens when trying to associate functions with type variables.


```
 Could not deduce (Typeable a0) arising from a use of ‘cast’
```
I use some heterogeneous lists in Nomyx (maybe an anti-pattern).
This lead to very troublesome error messages.


Edit: due to a lot of strong reactions on [Reddit](https://www.reddit.com/r/haskell/comments/5ixm49/blog_post_haskell_is_hard_aaaaaaaaaaahhhhhhhhh://www.reddit.com/r/haskell/comments/5ixm49/blog_post_haskell_is_hard_aaaaaaaaaaahhhhhhhhh/), I changed the section on "Don't use too much higher order functions" to "Don't use too much indirections".
