## InTree
Interval Trees in scala (ps they're immutable)

## motivation
I was going to do this for work, and I was pretty excited about it, but it
turned out that I didn't need to do it.  I was sort of disappointed, so I
decided to do it anyway in my free time.  This is the realization of that
idea.

## implementation

### overview
I'm implementing IntervalTrees as Red-Black trees where the key is the lower
bound of the Interval, and the order statistic is the greatest upper bound in
the subtree.  I am going to follow the CLRS RB Tree implementation fairly
closely, although I might switch over to AVL trees later.

### how much work has been done
Working implementation up and running, searches properly, but it's not balanced.
Also, all it does is interval trees, might be nice to have some kind of library
that makes augmenting trees super simple.  Already implement Iterable, so it's
officially interoperable with the scala collections library.

### how much more work has to be done
Definitely have to make it balanced.  Beyond that, I'd like to investigate
making it more efficient.  If I decide to make the order statistic more
versatile, I'll probably fork the project, since that seems sort of like a
different concern.  Might also be nice for the trees to be n-ary instead of
binary.

### highly experimental thoughts
Maybe reimplement using some kind of graph DSL, a la gremlin or some such.
Support ordered traversals maybe?