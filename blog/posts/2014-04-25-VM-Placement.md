---
title: Can Haskell save energy?
description: an exploration of the SBV library
tags: Haskell, Energy
---

I am maintaining the software [Plug4Green](https://github.com/fit4green/Plug4Green), which is aiming at saving energy in data centres.
It is based on [Constraint Programming](http://en.wikipedia.org/wiki/Constraint_programming) (CP), which is a programming paradigm devoted to solve Constraint Satisfaction Problems (CSP).
Plug4Green is written in Java, using the libraries [Choco](http://www.emn.fr/z-info/choco-solver/) and [BtrPlace](http://btrp.inria.fr/).

While CP was a very good choice and fulfilled most of the requirements, there are some practical drawbacks. 
The first being that the languages used, Choco and Java, are very verbose.
Defining a new constraint takes a lot of lines of code and is very error prone.
The debugging period for each new constraint is also quite long.
This diminishes the flexibility of the tool: creating a new constraints to fit a new requirement arriving in a data centre should be easy and short.

I started exploring alternatives to the couple Constraint Programming/Java, using my favorite programming language: Haskell.
Programs in Haskell tend to be much less verbose than in Java (in the order of ten time less lines).
It is also a declarative language, like Constraint Programming is, so the expression of constraints is more clear and natural.
Haskell is a pure language, which means that no function can have side effects.
Every function, for the same arguments, will always yield the same results.
This property (purity), combined with the strong type system of Haskell allow to reduce drastically the number of bugs.

In parallel, Satisfiability Modulo Theories (SMT) is an active research area, slightly different than Constraint Programming.
A SMT problem is the problem of determining the satisfiability of logical formulas with respect to background theories.
Modern SMT solvers integrate a Boolean satisfiability (SAT) solver with specialized solvers for a set of literals belonging to each theory.
The constraints are stated over specific domains, typically: Booleans, integers, rationals, reals, finite domains, or combinations of them.
The problem consists in finding an assignment to the variables that satisfy all constraints.

To show the usability of both SMT and pure functional languages to tackle energy efficiency problems, I implemented the classical problem of packing VMs on servers using the library [SBV](http://leventerkok.github.io/sbv/), with only one dimension for the sake of simplicity.
In the example underneath ([full code here](https://github.com/cdupont/Plug4Green-design)), each VM has a demand in term of CPU, and each server has a certain CPU capacity to offer.
The objective is to find the placement of the VMs on the servers that minimizes the number of servers needed.
The only constraint applied is that the total CPU consumption of the VMs that will be running on a server must not exceed the capacity of that server.

``` haskell
--concrete IDs for VMs and servers
type VMID = Integer
type SID = Integer

--symbolic IDs of the servers
type SSID = SBV SID

--A VM is just a name and a cpuDemand
data VM = VM { vmName :: String,
               cpuDemand :: Integer}

--a server has got a name and a certain amount of free CPU
data Server = Server { serverName :: String,
                       cpuCapacity :: Integer}

--list of VMs
vms :: Map VMID VM
vms = fromList $ zip [0..] [VM "VM1" 100, VM "VM2" 50, VM "VM3" 15]

--list of servers
servers :: Map SID Server
servers = fromList $ zip [0..] [Server "Server1" 100, Server "Server2" 100, Server "Server3" 200]

--number of servers ON (which we'll try to minimize)
numberServersOn :: Map VMID SSID -> SInteger
numberServersOn = count . elems . M.map (./= 0) . vmCounts

--computes the number of VMs on each servers
vmCounts :: Map VMID SSID -> Map SID SInteger
vmCounts vmls = M.mapWithKey count servers where
   count sid _ = sum [ite (mysid .== literal sid) 1 0 | mysid <- elems vmls]

--All the CPU constraints
cpuConstraints :: Map VMID SSID -> SBool
cpuConstraints vmls = bAnd $ elems $ M.mapWithKey criteria (serverCPUHeights vmls) where
   criteria :: SID -> SInteger -> SBool
   criteria sid height = (literal $ cpuCapacity $ fromJust $ M.lookup sid servers) .> height

--computes the CPU consummed by the VMs on each servers
serverCPUHeights :: Map VMID SSID -> Map SID SInteger 
serverCPUHeights vmls = M.mapWithKey sumVMsHeights servers where
   sumVMsHeights :: SID -> Server -> SInteger
   sumVMsHeights sid _ = sum [ite (sid' .== literal sid) (literal $ cpuDemand $ fromJust $ M.lookup vmid vms) 0 | (vmid, sid') <- M.assocs vmls]

--solves the VM placement problem
vmPlacementProblem :: IO (Maybe (Map VMID SID))
vmPlacementProblem = minimize' numberServersOn cpuConstraints

main = do
   s <- vmPlacementProblem   
   putStrLn $ show s
```

When run, this program returns the placement for the VMs that minimizes the number of necessary servers.
In this case, it will place all three VMs on the third server. 
While it is difficult to compare, it is anyway striking that this program is shorter than its equivalent in Java/Choco (for example [this implementation](http://www.dcs.gla.ac.uk/~pat/cpM/jchoco/binPack/CPBinPack.java) of bin packing).
The definition of a constraint takes only a few lines (for example `numberServersOn` takes 2 lines) and flows with the program definition.
Furthermore, as it is usually the case in Haskell, the type signature of the functions are carrying a lot of information that can be used both by the programmer to understand and reason about the program, and by the compiler to prove its correctness.
For example, the type signature `numberServersOn :: Map VMID SSID -> SInteger` makes it clear that the function `numberServersOn` is a constraint that takes the positions of all the VMs on the servers (denoted as a mapping between the VM ids and the server symbolic ids) and returns a symbolic integer representing the necessary number of servers.

Furthermore, programming at the symbolic level, as it is required when designing a CSP, is not very different than programming in concrete Haskell.
All we have to do to "switch" to the symbolic level is to prepend `SBV` to the concrete type: `SBV Integer` (SBV is a type function).
What is elegant is that a lot of the Haskell standard functions, like the function $sum$ in our example program, can be reused in a constraint programming program.
The definition of `sum` in the standard library of Haskell is generic enough to be used also at the symbolic level.
On the other hand, programming in Choco is completely different than programming in concrete Java: all the operators are necessarily different, due to the low genericity of Java.
Therefore, the intuition of the Java programmer cannot be completely reused.

SBV is also a theorem prover, and that can be used to prove properties of the constraints expressed.
For example, we might want to prove some properties about our constraint `vmCounts`.
This function counts the number of VMs present on each servers.
We want to prove the property that the count of VMs on a server has for absolute maximum the total numbers of VMs present in the data centre.
This is easily done, for example using [GHCi](http://www.haskell.org/haskellwiki/GHC/GHC), the interactive interpreter of Haskell.


    Main> prove $ \x y -> bAll (.<= 2) $ vmCounts' [x, y]
    Q.E.D.


This example shows how we can ask SBV to prove that the number of VMs per server computed by the constraint `vmCounts` cannot exceed the total number of VMs (in this simplified example with only 2 VMs and a version of vmCounts defined for lists instead of maps).
SBV simply replies with `Q.E.D`, showing that it found a proof for our property.
A proof is a much more powerful way to ensure the correctness of a program than testing: by testing a property of a program, one is effectively trying only a subset of all the possible behaviours of a program, while a proof is exhaustive (is fact, a proof corresponds to a superset of all program behaviours for a certain property).

What is left to do? Benchmarking, of course!
There is no guaranty that SMT/SBV will scale up with this type of problems.


Backlink: [Reddit](http://www.reddit.com/r/haskell/comments/247caq/blog_post_can_haskell_save_energy_vm_placement/)
