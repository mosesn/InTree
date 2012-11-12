## InTree
Interval Trees in scala

## motivation
I was going to do this for work, and I was pretty excited about it, but it
turned out that I didn't need to do it.  I was sort of disappointed, so I
decided to do it anyway in my free time.  This is the realization of that
idea.

## implementation
I'm implementing IntervalTrees as Red-Black trees where the key is the lower
bound of the Interval, and the order statistic is the greatest upper bound in
the subtree.  I am going to follow the CLRS RB Tree implementation fairly
closely, although I might switch over to AVL trees later.

Part of the point of doing this, for me, is to get a better handle on how the
Scala collections library works, so I'm implementing InTree as a collection,
and will be slowly extending it to make it more efficient, so that it doesn't
have to dump every element into a new builder every time I add something.
