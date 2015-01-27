---
title: The problem with Parallel Programming
description: 
tags: Programming
---

Parallel programming is hard.
The main problem for me is that parallel programming mixes two different things: the logic of a program, and its run-time behavior, together in the same language.
In traditional programming, the programmer usually doesn't care about the run-time evaluation strategy of her program.
If the language is imperative, the evaluation will be a step by step evaluation and execution of the instructions of the program.
If it's a functional language, the functions will be plugged together like pipes as a first step, and then the data will flow.
But the programmer didn't chose the model: it was imposed by the language framework.

However with parallel programming everything becomes more complicated.
Take a simple problem of counting words in a text.
[Here is the standard implementation with Hadoop](http://wiki.apache.org/hadoop/WordCount).
The example is quite long (63 lines): this is a side effect of Java and Hadoop technology choices.
But the real problem is not here: it's the fact that the program mixes program logic and program run time behavior description.
The program logic should be really short, even one or two lines should suffice to count words (here is the example in Haskell).

    wordCount :: String -> [(String, Int)]
    wordCount = map (head &&& length) . group . sort . words

However in the case of parallel programming this logic is broken up and drown into a swamp of runtime behavior code.

How to achieve the design goal of keeping the logic of a program separated from its run time behavior indications?
This is the real question that should be solved in parallel programming in my opinion.


