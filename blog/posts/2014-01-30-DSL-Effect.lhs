---
title: Manage Effects in DSLs
description: How to mark down and track effects in your DSLs
tags: Haskell
---

I have a small DSL for my [Nomyx](http://www.nomyx.net) game.    
Some instructions have effects (change the game state), some not.
In this blog post, we'll try to solve the following question: how can we semantically separate instructions with effect from the others? i.e. how can I mark down and track those effects?

Here is a simplified version of the DSL I use.
First some boilerplate (this is literate Haskell, you can run it):

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds, KindSignatures #-}
> import Control.Monad
> import Control.Monad.State

This is the DSL:
    
> data Exp a where
>   ReadAccount  :: Exp Int
>   WriteAccount :: Exp Int -> Exp () 
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

"victoryRule" sets the victory condition to be: "if there is more than 100 gold in the account, you win."

This is the game state:

> data Game = Game { bankAccount :: Int,
>                    victory     :: Exp Bool,
>                    timerEvent  :: Exp ()}

The evaluation of "Exp" can be:

> eval :: Exp a -> State Game a
> eval  (SetVictory v) = modify (\g -> g{victory = v})
> eval ReadAccount = get >>= return . bankAccount
> eval _ = undefined -- etc.

If you evaluate "victoryRule", you change the Game state by setting the victory field. Then, each time you will evaluate the victory field, you will know if you won or not (depending on your account...).
This is all well and good, but imagine if you write:

> victoryRule' :: Exp ()
> victoryRule' = SetVictory $ do
>   m <- ReadAccount
>   WriteAccount (return $ m + 1)
>   return (m > 100)

Ho no! Now each time a player is refreshing his screen (on the web interface), the victory condition is re-evaluated to be displayed again, and the bank account is increased by 1!
This is not what we want. We should allow only effect-less (pure) instructions in the victory field, like readAccount, but not WriteAccount.

Here is one solution. We need to separate the DSL instructions that have effect form the ones that have no effect. 
First, we need to define a data kind, called Eff. This is done by defining a data type Eff: with the pragma DataKind it be automatically promoted to kinds.
 
> data Eff = Effect | NoEffect

> -- first type parameter is used to track effects
> data Exp' :: Eff -> * -> * where
>   ReadAccount'  :: Exp' r Int  --ReadAccount can be used in whatever monad
>   --WriteAccount takes an effect-less expression, and returns an effectfull expression
>   WriteAccount' :: Exp' NoEffect Int -> Exp' Effect ()  
>   SetVictory'   :: Exp' NoEffect Bool -> Exp' Effect ()
>   --OnTime can trigger whatever expression, in particular effectful ones
>   OnTimer'      :: Exp' Effect () -> Exp' Effect () 
>   Const'       :: a -> Exp' r a
>   Bind'         :: Exp' r a -> (a -> Exp' r b) -> Exp' r b



> main = undefined
> instance Monad (Exp' a) where
>    return = Const'
>    (>>=)  = Bind'


That time, we can re-write victoryRule:

> victoryRule'' :: Exp' Effect ()
> victoryRule'' = SetVictory' $ do
>   m <- ReadAccount'
>   --WriteAccount (return $ m + 1) --won't compil (good)!
>   return (m > 100)

We cannot add effectful instructions in the victory condition anymore: mission accomplished!

We can also define a timer. Unlike SetVictory, OnTimer accepts effectful instructions. myTimer set a rule that will increment my bank account of 1 gold every minute:

> myTimer :: Exp' Effect ()
> myTimer = OnTimer' $ do
>   m <- ReadAccount'
>   WriteAccount' (return $ m + 1)

