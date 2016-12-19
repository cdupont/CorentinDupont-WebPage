---
title: How to structure your Haskell program 
description: Haskell program structure 
tags: 
---

How to structure medium or big Haskell programs?
Through my journey using Haskell, I devised some basic structures.
See also [my post](2016-08-06-Haskell-hard.html) on "donts" with Haskell.
You can find the source code of this blog post on [github](https://github.com/cdupont/haskell-program-structure).

Basic program structure
=======================

This is my basic program structure:

```
- README.md
- myprog.cabal
- stack.yaml
- AUTHORS
- LICENCE
- TODO
- src
  - Myprog
    - Main.hs
    - Game.hs
    - Settings.hs
    - Types.hs
    - Utils.hs
    - SubPart.hs
    - SubPart
      - Thing.hs
      - Types.hs
      - Utils.hs
```

Use a Types module
------------------

I found that defining a "Types" module, containing all the types of your program, is helpful.
See for example [this module](https://github.com/cdupont/Nomyx/blob/master/Nomyx-Language/src/Nomyx/Language/Types.hs).
On the contrary, most libraries that you find on Hackage defines the types in the [same files](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Maybe.html) where they are used.
However, I found that putting all the types in the same "Types" module allows to give a big picture of the program.
Furthermore it helps avoids cycle dependencies.

Example of Types.hs file:
```haskell
{-# LANGUAGE TemplateHaskell    #-}

module Types where

import Control.Lens

-- | Informations on a particular game 
data Game = Game { _gameName :: String,
                   _settings :: Settings}
                   deriving (Show, Eq)

-- | settings
data Settings = Settings { _login  :: String, 
                           _avatar :: FilePath}
                           deriving (Show, Eq)

makeLenses ''GameInfo
makeLenses ''Settings
```

This files defines the Game and the Settings data structures.
You can immediatly see the dependencies between the two.
The functions working on these can be defined in different files, for instance `Game.hs` and `Settings.hs`.
 

State Monad with Lens
---------------------

Structure your program around a State Monad, and access it via Lenses.
I found it the most efficient way of structuring big programs.
Each functions can deal only with a sub-part of the data structure.

Using the example above, here is the `Game.hs` file:

```haskell
module Game where

import Settings

startGame :: StateT Game IO ()
startGame = do
  liftIO $ putStrLn "Starting Game"
  res <- zoom settings checkSettings
  if res 
    then putStrLn "Settings OK"
    else putStrLn "Wrong settings"
```

And the `Settings.hs` file:
```haskell
module Settings where

checkSettings :: StateT Settings IO Bool
checkSettings = do
   log <- use login
   return (length log /= 0)
```

I use a [StateT monad transformer](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html) to express the type of my function: `StateT Settings IO Bool`.
The `Settings` is the data structure containing the state. 
Using `IO` as a monad allows you to perform additional IO operations with `liftIO`.
If you don't need IO in you operations, use simply `State Settings Bool`.
The `Bool` is the return type, in case your function needed to return something.
To access/update the state, use [Lenses](https://github.com/ekmett/lens/wiki/Operators).
Lenses allows you to access you data structure in a [very handy way](http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html).
For small programs it's overkill, but as soon as your data structure will grow bigger with many levels of depth, it's absolutly necessary.


Utils module
------------

Another useful module is "Utils".
I put here all functions that does not fit anywhere basically, but are of general utility, for example [concatMapM](https://github.com/cdupont/Nomyx/blob/master/Nomyx-Language/src/Nomyx/Language/Utils.hs).
It's cleaner to avoid this module it of course, if you can, but not always possible...

Split your program as you go
----------------------------

Start small and, as soon as your program gets too big, split it.
200 lines of Haskell is already quite a lot for the same file.
In the same order of ideas, when the program gets too big for a single package, consider creating some libraries.
A package with 20 .hs files is already quite a lot, in my opinion.


Use re-exports
--------------

If some part of your program becomes big, it makes sense to create a sub-folder, or even a library.
In both cases I create [a module](ihttps://github.com/cdupont/Nomyx/blob/master/Nomyx-Core/src/Nomyx/Core/Engine.hs) with the same name as the folder/library, containing re-exports.
You can re-export only the functions that are useful to your users, leaving all internal functions hidden.
That allows to:

- allows users of your library/program to import it with one `import`, instead of importing each modules.
- hide the internals.

This module contains only exported function names and the imported modules:
```
module Engine (gravity,
               rendering) where

import Engine.Mechanics
import Engine.Rendering
```

Use type synonyms
-----------------

Type synonyms are very useful to avoid confusion and potential bugs.
I usually define type synonyms when my functions use common types such as `String` or `Int` as parameters.

```
type GameName = String
type PlayerName = String

addPlayerToGame :: PlayerName -> GameName -> StateT Game IO ()
addPlayerToGame pn gn = ...
```
Using the type synonims can avoid confusing a player name and a game name.
Since the visual information is carried by the types, the variable names can stay short.
If you don't use the type synonyms, you need to keep long variable names such as `playerName` and `gameName`.


