---
title: Inside the Internet of Things: Conduits and Pipes
description: Bringing some concepts from Haskell to IoT
tags: IoT, Haskell
---

#Introduction

The vision of the [Internet of Things](http://en.wikipedia.org/wiki/Internet_of_Things) (IoT) is a world where physical objects are seamlessly integrated into the information network, and where the physical objects can become active participants in business processes (definition by SAP).
IoT proposes to connect a lot of objects to the internet. This is something that we can consider as achieved: see the [Xively](https://xively.com/) platform for example. 
The real challenge is now to build something useful out of all these data streams.
We need a paradigm powerful enough to build meaningful, flexible and persistent "machines" able to extract high level informations out of all the low level data streams that we have at our disposal.
The objective of this blog post is to present the concepts introduced by the Haskell [Pipes](https://github.com/Gabriel439/Haskell-Pipes-Library) library, with an argumentation geared toward the IoT community.
It is freely inspired from this [tutorial](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Tutorial.html). Similar ideas are implemented in the [Conduit](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview) library.


To process data streams, we usually use a data streaming network.
The processing nodes of this network should have the following qualities:

* Streaming capabilities
* Composability & Flexibility
* Management of effects

##Streaming capabilities

This feature is obviously needed for a streaming network. It is however non trivial to implement correctly.
The implementation should be able to deal with large -and possibly infinite- amounts of data, in constant memory.
It implies to implement the following mechanisms:

* Event management or push/pull mechanism: the new data from the source may come at any moment. 
As this moment is unpredictable, each network node shall be "woken up" by the new data coming, instead of actively polling its input buffer.
* Management of end of streams: the source can send an end of stream signal (with the character EOF). This should be handled gracefully by all the network nodes, and terminate the entire network.
* Management of errors: any network node can terminate with a "broken pipe" error, for example in the case the sink is switched off, or receives data that it cannot handle. 
The "broken pipe" error should be propagated to all network nodes to allow them to gracefully terminate.

##Composability & Flexibility

Each network nodes should be easily "pluggable" onto other network nodes. 
A network node should embed in it interface the specification of the type of data it can get as an input, and the type of data it can output. 
The compiler will then ensure the compatibility of each network node connection: a connection between an output and an input of two different types should be rejected by the compiler.
Furthermore, the types of the inputs and outputs should be the most generic possible: this will ensure the reusability of a network node.
The compiler should also be able to deduce the data type outputed by the whole streaming network, taking into account every transformations.

##Management of effects

It is very important to manage the effects in a stream processing network. 
An effect, from the point of view of a function, is a direct modification of its environment (for example modifying a global variable or printing something on the screen).
Purity is globally desirable for a network node: a pure (effectless) function is always easier to reason about, less prone to bugs, and easier to compose.
Indeed, the output of a network node should only depend on its input, and not any environment state, which would make it much more difficult to manage and reuse.
Effects are nevertheless necessary at certain points of an otherwise pure network, especially at the sources and sinks. 


#Implementation in the Pipes library

The [Pipes](https://github.com/Gabriel439/Haskell-Pipes-Library) library in Haskell implements data streaming programming in a very neat way.
This library offers the necessary theoretical toolbox and its implementation. 
The Pipes library proposes several processing units, among which:

* [Producers](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Core.html#t:Producers) (the sources)
* [Consumers](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Core.html#t:Consumer) (the sinks)
* [Pipes](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Core.html#t:Pipes)

To make all those three easily composable, the design trick is that they all derive from the same underlying type, called a [Proxy](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Core.html#t:Proxy).
You can think of a Proxy as a box with an input _a_ and an output _b_:

            Proxy
         +---------+
         |         |
     a  ==>       ==> b
         |    |    |
         +----|----+
              v
              r
It's (simplified) type is:

    type Proxy a b m r = <some implementation>

_a_ _b_ _m_ and _r_ are _type variables_. This means that a Proxy can be configured to use any type you choose. 
Respectively, _a_ is the type of the input data, _b_ is the type of the output data, _m_ is the underlying [monad](http://en.wikipedia.org/wiki/Monad_%28functional_programming%29) (a concept that we won't cover in this tutorial), and _r_ it the type of the data handed out when the Proxy terminates (think of it as a return status).
An concrete usage of a Proxy could be:

    MyProxy = Proxy String String Foo Int

_MyProxy_ is then a Proxy that takes a String as an input and output a String. When it terminates, it returns an Int.

A Producer is then just a specialisation of a Proxy, with its input type set to unit (denoted "()"):

    type Producer b = Proxy () b

          Producer
         +---------+
         |         |
         |        ==> b
         |    |    |
         +----|----+
              v
              r

This type signature means that a Producer can only output values. These values can be of any type, that's why we write it "b" instead of a concrete type like Int of Float.
The input is set to the special type "()", pronounced "unit". "()" is a type that contains only one value, and thus naturally no (interesting) data can be sent to the Producer.
"()" for our purpose serves to "close" one end of the Proxy.

Conversely, a Consumer is a Proxy with its output type set to ():

    type Consumer a = Proxy a ()

          Consumer 
         +---------+
         |         |
     a  ==>        |
         |    |    |
         +----|----+
              v
              r

Finally, a Pipe has the full feature (an input "a" and an output "b"):

    type Pipe a b = Proxy a b


In fact, I lied a little about the Proxy: its type is slightly more complex. It also contains a upstream interface for data coming the opposite way (denoted with a' and b'):

    data Proxy a' a b' b m r 

            Proxy
         +---------+
         |         |
     a' <==       <== b'
         |         |
     a  ==>       ==> b
         |    |    |
         +----|----+
              v
              r

This allows to define a Server and a Client:

    type Server b' b = Proxy () () b' b

           Server
         +---------+
         |         |
         |        <== b'
         |         |
         |        ==> b
         |    |    |
         +----|----+
              v
              r

    type Client a' a = Proxy a' a () ()

            Client
         +---------+
         |         |
     a' <==        |
         |         |
     a  ==>        |
         |    |    |
         +----|----+
              v
              r

The fundamental difference between a Server and a Client is that a Client initiate the communication (in a pull-based approach).

Finally, an Effect is a special kind of network node that have no input and no output:

    type Effect = Proxy () () () ()

            Effect
         +---------+
         |         |
         |         |
         |    |    |
         +----|----+
              v
              r

Effects are used for non streaming components. As we'll see below, a whole network should always reduce to an Effect.

The library is then defining various operators to connect network nodes together: [for](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes.html#v:for), [(>~)](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes.html#v:-62--126-), [(>->)](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes.html#v:-62--45--62-), [(>>=)](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Monad.html#v:-62--62--61-)
The idea is that, using those operators, you can connect the various network nodes showed above.
For example, you can connect a Producer that produces Strings, a Pipe that modify those Strings, and finally a Consumer of Strings:

           Producer                Pipe                 Consumer
         +-----------+          +----------+          +------------+
         |           |          |          |          |            |
         |  stdinLn  |          |  take 3  |          |  stdoutLn  |
         |          ==> String ==>        ==> String ==>           |
         |     |     |          |    |     |          |      |     |
         +-----|-----+          +----|-----+          +------|-----+
               v                     v                       v
               ()                    ()                      ()

Those 3 boxes fuses into just one, and can be seen as only one box of type Effect, with no input and no output:

                       Effect
        +-----------------------------------+
        |                                   |
        |                                   |
        |  stdinLn >-> take 3 >-> stdoutLn  |
        |                                   |
        |                                   |
        +----------------|------------------+
                         v
                         ()

Here is the code to produce this network:

    runEffect $ stdinLn >-> take 3 >-> stdoutLn

We simply connect a Producer (named stdinLn) with a Pipe (take 3) and finally with a Consumer (stdoutLn) using the (>->) operator.
Only networks that reduces to an Effect can be run by the engine (otherwise, that would mean that some inputs or outputs are left unplugged).
When run, this example reads for the standard input and immediately displays what has been entered, only to stop (and terminate) after 3 inputs:

    $ runEffect $ stdinLn >-> take 3 >-> stdoutLn
    Foo<Enter>
    Foo
    Bar<Enter>
    Bar
    Baaz<Enter>
    Baaz
    <Terminated>


# Behind the scene: how to ensure composability?

Pipes is a principled library using the [Category theory](http://en.wikipedia.org/wiki/Category_theory).
Category theory is a field used to formalize mathematics and its concepts as a collection of objects and arrows (also called morphisms).
Out of this theory, a "Category" is a set (in the mathematical sense) of things that, put simply, can "compose". 
When creating a programming library, the usual way to make it "composable" is by trial and error, and by intuition.
However, using the category theory as a design principle can be a way to ensure (and demonstrate) a great "composability".
The category design pattern is explained by Gabriel Gonzalez [here](http://www.haskellforall.com/2012/08/the-category-design-pattern.html).

As an example, the set of functions forms a Category, because functions can compose: if you take two functions f and g, you can possibly compose them to obtain a new function h:

    f :: a -> b  -- read this as "f is a function that takes an argument of type a, and returns a result of type b"
    g :: b -> c
    h :: a -> c
    h =  g . f  -- h is the composition of f and g

But there are other things, apart functions, that are an instance of Category: [Lenses](http://hackage.haskell.org/package/data-lens), [Parsers](http://hackage.haskell.org/package/boomerang-1.4.0), and... Pipes.
A set of objects "cat" forms a Category where:

    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

This says that there is an object in your set "cat" that is named the identity. Secondly we have an operator "." that is able to compose your objects, very much like we compose functions.
Furthermore, your Category must obey the Category laws:

    id . f = f  -- Left  identity law
    f . id = f  -- Right identity law
    (f . g) . h = f . (g . h) -- Associativity law

If your set of objects "cat" respects all these laws, it is a Category. 
In practice it is very useful, when designing a library, to make sure that a certain set of the primitives defined by your library obey the Category laws. 
This ensures that those primitives combines nicely and intuitively, and are free of edge cases.

Pipes is a stream programming library built on top of a foundation of basic category theory. The core of the library consists of a set of five categories that all intersect in a single streaming data type and the library's contract is the set of associated category laws.
The five categories are given [here](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Core.html#g:2).

#Conclusion

The Pipes library helped us understand some machinery which is necessary for the Internet of Things. The library offers also an implementation, of course.
There is much more to Pipes, check out the [tutorial](http://hackage.haskell.org/package/pipes-4.0.0/docs/Pipes-Tutorial.html). 
The [Conduit](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview) library shows similar ideas, although the implementation seems simpler.

#Acknowledgments

Gabriel Gonzales is the author of Pipes, so I thank him for this great contribution and for the feedback on this blog post!




