---
title: Manage Effects in DSLs
description: How to mark down and track effects in your DSLs
tags: Haskell
---

I have a small DSL for my [Nomyx](http://www.nomyx.net) game.    
Some instructions have effects (change the game state), some not.
In this blog post, we'll try to solve the following question: how can we semantically separate instructions with effect from the others? i.e. how can I mark down and track those effects?

To show you the problem, here is a simplified version of the DSL I use.
First some boilerplate (this post is literate Haskell, you can copy/paste it and run it as-is):

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds, KindSignatures #-}
> module DSLEffects where
> import Control.Monad.State

This is the DSL:
    
> data Exp a where
>   ReadAccount  :: Exp Int
>   WriteAccount :: Int -> Exp () 
>   SetVictory   :: Exp Bool -> Exp ()
>   OnTimer      :: Exp () -> Exp ()
>   Const        :: a -> Exp a
>   Bind         :: Exp a -> (a -> Exp b) -> Exp b

It can read and write to an account (belonging to the state of the game), set a victory condition, and set an expression to be triggered every minute. The Monad instance is pretty straightforward:

> instance Monad Exp where
>    return = Const
>    (>>=)  = Bind

With that you can write:

> victoryRule :: Exp ()
> victoryRule = SetVictory $ do
>   m <- ReadAccount
>   return (m > 100)

_victoryRule_ sets the victory condition to be: "if there is more than 100 gold in the account, you win."

This is the game state:

> data Game = Game { bankAccount :: Int,
>                    victory     :: Exp Bool,
>                    timerEvent  :: Exp ()}

It contains your amount of gold, the victory condition, and the expression that must be triggered every minute by the system. Note that the field _victory_ is not a boolean: it's an expression. 
The evaluation of _Exp_ can be:

> eval :: Exp a -> State Game a
> eval  (SetVictory v) = modify (\g -> g{victory = v})
> eval ReadAccount = get >>= return . bankAccount
> eval _ = undefined -- etc.

If you evaluate _victoryRule_, you change the _Game_ state by setting the victory field. Then, each time you will evaluate the victory field, you will know if you won or not (depending on your account...).
This is all well and good, but imagine if you write:

> victoryRule' :: Exp ()
> victoryRule' = SetVictory $ do
>   m <- ReadAccount
>   WriteAccount (m + 1)
>   return (m > 100)

Ho no! Now each time a player is refreshing his screen (on the web interface), the victory condition is re-evaluated to be displayed again, and the bank account is increased by 1!
This is not what we want. We should allow only effect-less (pure) instructions in the victory field, like _readAccount_, but not _WriteAccount_.

Here is one solution. We need to separate the DSL instructions that have effects from the ones that have no effect. 
First, we need to define a data kind, called _Eff_. This is done by first writing a data type _Eff_, and then promoting it to the kind level. This is accomplished automatically with the pragma [DataKind](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/kind-polymorphism-and-promotion.html).
 
> data Eff = Effect | NoEffect

Then, we specify that the first type parameter of an expression must be of kind _Eff_, to allow us to mark effects:

> data Exp' :: Eff -> * -> * where
>   ReadAccount'  :: Exp' r Int  --ReadAccount can be used in whatever monad
>   --WriteAccount takes an effect-less expression, and returns an effectfull expression
>   WriteAccount' :: Int -> Exp' Effect ()  
>   SetVictory'   :: Exp' NoEffect Bool -> Exp' Effect ()
>   --OnTime can trigger whatever expression, in particular effectful ones
>   OnTimer'      :: Exp' Effect () -> Exp' Effect () 
>   Const'        :: a -> Exp' r a
>   Bind'         :: Exp' r a -> (a -> Exp' r b) -> Exp' r b
>
> instance Monad (Exp' a) where
>    return = Const'
>    (>>=)  = Bind'

Each instruction of our language can now specify if it allows effects or not in its parameters and return type. That time, we can re-write _victoryRule_:

> victoryRule'' :: Exp' Effect ()
> victoryRule'' = SetVictory' $ do
>   m <- ReadAccount'
>   --WriteAccount (m + 1) --won't compil (good)!
>   return (m > 100)

We cannot add effectful instructions in the victory condition anymore: mission accomplished!

We can also define a timer. Unlike _SetVictory_, the _OnTimer_ instruction accepts effectful instructions as a parameter. _myTimer_ hereunder sets a rule that will increment my bank account by 1 gold every minute:

> myTimer :: Exp' Effect ()
> myTimer = OnTimer' $ do
>   m <- ReadAccount'
>   WriteAccount' (m + 1)

