---
title: Data migration for ACID state using GIT 
description: 
tags: Haskell 
---

The problem
-----------

How to avoid keeping old data formats in your code?
Indeed those old data formats are often needed for retro-compatibility with previous versions of your software.

Say you have a data structure that is serialized in a file, and then you add a field in a later version.
You are obliged to keep both versions of the data structure if you want to be able to read both versions of the file.
Potentially, if you update very often your structure between releases, you are obliged to keep N versions of the data structure in your code.

See the exemple at: http://acid-state.seize.it/safecopy
In this example you can see the problem: the author is obliged to keep old code (data structures) to maintain compatibility. Even worth, you might be obliged to suffix your data structure with a version number:

    data MyType_V1 = MyType_V1 Int
    data MyType_V2 = MyType_V2 Integer

Ideas
-----

Instead, I'm thinking of a process using GIT, or Cabal as a back-end.
The idea would be to have an additional program (or library) specialized in the data migration of your main software.
It would extract both version A and B from the repo, and then would compile an application capable of handling migrations from FA to FB.

Implementation
--------------

1. Use a custom Setup.hs that extracts specific versions of the types from GIT
2. rename the module in those files to suffix the version
3. import qualified those modules in the current types file



