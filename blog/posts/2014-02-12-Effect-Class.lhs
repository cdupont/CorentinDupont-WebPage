---
title: Manage effects in DSLs (part 2)
description: How to manage effects in a DSL, using type classes
tags: Haskell, Nomyx
---

Hi, this is the second post in the exploration of the design of [Nomyx](www.nomyx.net).
We will try to solve again the problem of managing effects in DSLs, this time using type classes (see [part 1](/blog/posts/2014-01-30-DSL-Effect.html) to see how to solve it using a monadic parameter).
Has a reminder, the problem is to separate semantically the instructions that have an "effect" in a DSL, from those who don't.
Of course, an effect-less instruction could be run in an effect-full context, but not the opposite!
How to encode this semantic?

Preliminaries: 

> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeSynonymInstances #-}
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

> victoryCondition :: Nomex m => m ()
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
