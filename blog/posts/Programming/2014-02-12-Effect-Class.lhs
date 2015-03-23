---
title: Manage effects in DSLs (part 2)
description: How to manage effects in a DSL, using type classes
tags: Haskell, Nomyx
---

Hi, this is the second post in the exploration of the design of [Nomyx](http://www.nomyx.net).
We will try to solve again the problem of managing effects in DSLs, this time using type classes (see [part 1](2014-01-30-DSL-Effect.html) to see how to solve it using a polymorphic type parameter).
As a reminder, the problem is to separate semantically the instructions that have an "effect" in a DSL, from those who don't.
Of course, an effect-less instruction could be run in an effect-full context, but not the opposite!
How to encode this semantic?

Preliminaries: 

> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE FlexibleInstances #-}
> module Main where
> import Control.Monad.State
> import Control.Monad.Reader

We define a class Nomex, which will hold effectless instances of a Nomex language. 

> class Monad m => Nomex m where
>   readAccount :: m Int

We also define a class NomexEffect, for effectfull versions of the language. Note that the effectful version contains the effectless one.

> class Nomex m => NomexEffect m where
>   writeAccount :: Int -> m ()
>   setVictory   :: (forall n. Nomex n => n Bool) -> m ()

Here is the state of the game. We can set a victory rule, that is run will tell us if we won. We can also have money on our account!

> data Game = Game { victory :: (forall m. Nomex m => m Bool)
>                  , account :: Int
>                  }

Here is where is becomes interesting. Our language is dedicated to read (for the effectless part) and modify (for the effectful part) the game state.
So here is the instance of the language to read the game state:

> instance Nomex (Reader Game) where
>   readAccount = asks account

And here is it for stateful computation. We define an instance of NomexEffect for State Game: 

> instance NomexEffect (State Game) where
>   writeAccount n = modify $ \game -> game { account = n }
>   setVictory   v = modify $ \game -> game { victory = v }

We define it also for effectless computations:

> instance Nomex (State Game) where
>   readAccount = liftEval readAccount 

> liftEval :: Reader Game a -> State Game a
> liftEval r = get >>= return . runReader r 

We are now able to define effectful computations, mixing effectful and effectless instructions:

> incrAccount :: NomexEffect m => m ()
> incrAccount = do
>    a <- readAccount 
>    writeAccount (a + 101)

We can also safely define expressions that must not yield any effect. 
Here is our victory condition. Note that effectful instructions are not accepted, which is what we wanted!

> victoryCondition :: Nomex m => m Bool
> victoryCondition = do
>    i <- readAccount
>    --writeAccount 100 --This would not compile (good!)
>    return (i > 100)

> winOnBigMoney :: NomexEffect m => m ()
> winOnBigMoney = setVictory victoryCondition 


All this allows us to define a pure function able to determine if we won the game:

> isVictory :: Game -> Bool
> isVictory g = runReader (victory g) g

We can now play! We first define how to win the game. The condition will be stored in the game state as an expression. We then increment the bank account.

> play = do
>    winOnBigMoney
>    incrAccount

Let's check that everything worked correctly:

> initGame = Game (return False) 0
> main = putStrLn $ show $ isVictory $ execState play initGame

Running the program will display the value "True": We won!

#Conclusion

So finally, there are really 3 solutions to the problem of representing the semantic of effects/no effects (effect-less instructions can be run in effect-full context, but not the opposite).
We can encode this semantic at (click on the links for full solutions):

* [value level](https://github.com/cdupont/Nomyx-design/blob/master/TypeParamEffect-Concrete.hs)
* [type level](https://github.com/cdupont/Nomyx-design/blob/master/TypeParamEffect-Polymorphic.hs)
* [typeclass level](https://github.com/cdupont/Nomyx-design/blob/master/TypeClassEffect.hs)

At value level, the semantic is encoded by a DSL instruction 'NoEff' that wraps an effect-less instruction into an effect-full one:

    NoEff        :: Nomex NoEffect a -> Nomex Effect a

At type level, the semantic is encoded by using a polymorphic type parameter that can take two concrete type, 'Effect' or 'NoEffect':

    ReadAccount  :: Nomex r Int

At typeclass level, the semantic is encoded by the hierarchy of classes:

    class NomexNoEffect m => NomexEffect m where...


Here are their pros and cons, in my opinion:

**Value level solution:**

Pros:

* Less modification to the existing code base (I can just wrap the existing Nomex type as a type synomym).
* The type of the instructions of the DSL is very clear and elegant: *ReadAccount :: Nomex NoEffect Int*.

Cons:

* We have to lift every effect-less instructions in effect-full context.

**Type level solution:**

Pros:

* Less modification to the existing code base (I can just wrap the existing Nomex type as a type synomym)
* No need to lift anything

Cons:

* The type of effect-less instructions is no so elegant due to the polymorphic parameter IMO: *ReadAccount :: Nomex r ()*.
* The evaluator contains redundancy

**Type class solution:**

Pros:

* Elegant solution for the evaluation
* Effect-less and effect-full instructions can be used together in effect-full context, without need to lift anything

Cons:

* Two functions that "belong" together, *ReadAccount* and *WriteAccount*, and separated.
* The type signature less obvious for the players: *readAccount :: Nomex m => m Int*. I am a bit worried that all the gameplay will have to be done with type classes and not with a concrete type.
* The amount of change in the code base


Special thanks to the Haskell community who helped me. 
Especially the persons in [this Reddit post](http://fr.reddit.com/r/haskell/comments/1wd5z4/manage_effects_in_a_dsl/) and [this mailing list thread](https://groups.google.com/forum/#!msg/haskell-cafe/LzRIUQ-hM4c/4KcWae7XOZQJ).
