---
title: The smooth operator 
description: The role of spaces in programming languages
---

In programming languages, the role of spaces, or simple juxtaposition of elements can be quite revealing.
The simple juxtaposition of two elements from the language, without any visible operators in the middle is indeed the shortest grammar possible.

In functional programming
=========================

In Haskell, the whitespace can be seem as the "application operator".
Indeed, `f a b` is the application of function `f` to its arguments `a` and `b`.
It is the most terse writing possible for function application.
Compare it with the less terse notation chosen for Java for example: `f(a,b)`.
That's three non-space characters more.
It seems that Haskell chose the whitespace character to perform the main operation in functional programming: using functions.
 
In Math
=======

In Math, the simple juxtaposition of two variables often means multiplication.
For example, `f=2a+ab` mean `f` equals `2` times `a` plus `a` times `b`.
Again, the smallest operator possible (that is, no operator at all) has been devoted to the most prominent operation of the language.
 
In stack based languages
========================

In stack-based languages such as Forth, the whitespace character has been devoted to separate the elements on the stack.
For example `1 2 +` puts successively the numbers `1` and `2` on the stack, and then the operator `+`.

Human languages also follows a process of choosing the shortest forms for the most common words.
For example articles are often very short, down to one letter like the article `a`.
When designing a programming language, the designers chose carefully those single letter operators, because there can be only a few.
Regarding the juxtaposition or space operator, there can be only one so it should be the most common operation of the language.


More reads:    
http://www.storytotell.org/essays/juxtaposition.html    
http://www.cs.utexas.edu/~wcook/anatomy/anatomy.htm    
