---
title: A Cretan Maze using Haskell Diagrams
description: My first stab at using Haskell Diagrams
tags: Haskell
---

Let's create mazes with Haskell!! I want to realize a [Cretan maze](http://plus.maths.org/content/maths-amazes), the same in which the Minotaur got lost, using the great [diagrams](http://projects.haskell.org/diagrams/) library.
Here is the final picture:
 
![Cretan maze](/images/maze.svg "Cretan Maze")

Hypnotic, isnt't it?
Let's go though the program used to generate this labyrinth.
First thing I noticed: it is surprisingly hard! Masterizing the library took me quite a long time, and solving this particular problem even more. However Diagrams comes with a very complete documentation and several very good tutorials.

Let's begin with the usual preliminaries:

> {-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, TupleSections #-}
> module Main where
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.TwoD.Offset

A Cretan maze is built from an initial drawing and a seed.

![Initial drawing](/images/initDraw.svg "Initial drawing")

First we have to draw the initial diagram, in red in the picture. We can notice it has a rotation symetry, so we need to define only a quarter of it.
:

> initpts :: [P2]
> initpts = map p2 [(2,0),(2,1),(2,2),(1,2)]

Then we draw the segment and the arc:

> drawlines, drawarcs :: Diagram B R2
> drawlines = fromVertices $ map p2 [(0,0), (0,2)]
> drawarcs = translate (2*unitX + 2*unitY) $ arc (0.5 @@ turn) (0.75 @@ turn)

Finally we assemble everything and rotate it 4 times to obtain the complete figure:

> rep4 :: (Transformable t, V t ~ R2) => t -> [t]
> rep4 a = map (flip rotateBy a) [0, 1/4, 2/4, 3/4]
> 
> initdraw :: Diagram B R2
> initdraw = mconcat $ rep4 $ drawlines <> drawarcs
>
> allpts :: [P2]
> allpts = mconcat $ rep4 initpts

Here comes our first arc. We decide to draw it on the edge.

> arc0 :: Located (Trail R2)
> arc0 = translate (r2 (2, 1+1/2)) (arc' (0.5) (-0.25 @@ turn) (0.25 @@ turn)) 

We have then to remark that each arc is an offset of the previous, smaller arc:
   
> offsetArc :: Located (Trail R2) -> Located (Trail R2) 
> offsetArc arcP = offsetTrail 1 arcP

An offset is not enough when the curve reachs the corners: in this case, we need to add an additional "cap" on one of the ends. That's the role of capStart and capEnd, which take a curve, a rotation point and a final point, and draw the necessary cap at the end of the curve.

> capStart :: Located (Trail R2) -> P2 -> P2 -> Trail R2
> capStart lt startPt center = if (close 0.0001 startPt (atStart lt)) 
>   then mempty
>   else capArc 1 center startPt (atStart lt) 
>
> capEnd :: Located (Trail R2) -> P2 -> P2 -> Trail R2
> capEnd lt endPt center = if (close 0.0001 endPt (atEnd lt)) 
>    then mempty
>    else capArc 1 center (atEnd lt) endPt                    
>
> -- arcWithCaps is the concatenation of an offset with its start and end caps
> arcWithCaps :: Located (Trail R2) -> P2 -> P2 -> P2 -> P2 -> Located (Trail R2)
> arcWithCaps arcP startP startR endP endR = 
>    mconcat [capStart offs startP startR, 
>             unLoc $ offsetArc arcP, 
>             capEnd offs endP endR] `at` startP where
>       offs = offsetArc arcP

Finally, we are able to write the recursive function that draw the arc number n, based on the previous arc.
We pass as argument the list of points that we want as a start point respectively for each arc.

> arcN :: [P2] -> Int -> Located (Trail R2)
> arcN _  0 = arc0
> arcN ap n = arcWithCaps' (arcN ap (n-1)) (drop (n-1) ap) (drop (n-1) (reverse ap))

We can now put together all the arcs (8 arcs in total)!

> allArcs :: [Located (Trail R2)]
> allArcs = map (arcN $ shiftList 2 allpts) [0..7]
>
> res :: Diagram B R2
> res = initdraw <> (mconcat $ map strokeLocTrail allArcs)
>
> main = mainWith $ rotateBy (1/4) $ (res # lw 0.1 # lc green :: Diagram B R2)    


Following is some helper functions.

> shiftList :: Int -> [a] -> [a]
> shiftList n as = (drop n as) ++ (take n as)
>
> close eps a b = a `distanceSq` b <= eps*eps
>
> -- | Builds an arc to fit with a given radius, center, start, and end points.
> -- --   A Negative r means a counter-clockwise arc
> capArc :: Double -> P2 -> P2 -> P2 -> Trail R2
> capArc r c a b = trailLike . moveTo c $ fs
>    where
>       fs | r < 0     = scale (-r) $ arcVCW (a .-. c) (b .-. c)
>          | otherwise = scale r    $ arcV   (a .-. c) (b .-. c)
>
> -- Arc helpers
> arcV :: (TrailLike t, V t ~ R2) => R2 -> R2 -> t
> arcV u v = arc (direction u) (direction v)
>
> arcVCW :: (TrailLike t, V t ~ R2) => R2 -> R2 -> t
> arcVCW u v = arcCW (direction u) (direction v)
>
> arcWithCaps' arcP (p1:p2:_) (r1:r2:_) = arcWithCaps arcP r2 r1 p2 p1
> arcCWithCaps' _ _ _ = error "lists must contain at least 2 points each"

