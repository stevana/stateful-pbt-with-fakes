# Todo

Here are a bunch of idea I think are worth exploring:

1. We can also improve the shrinking of parallel commands by moving commands
   outside of forks, e.g. `[Fork [a, b]] ==> [Fork [a], Fork [b]]`, thus making
   the program more sequential, or by moving forks after smaller forks, e.g.
   `[Fork [a, b], Fork [c]] ==> [Fork [c], Fork [a, b]]`, thus making for less
   potential concurrent interleavings. Another thing that could be interesting
   to investigate is whether a property-based testing library with integrated
   shrinking, such as Python's hypothesis or Haskell's
   [`falsify`](https://hackage.haskell.org/package/falsify), can do the above
   parallel command shrinking or simplify shrinking in general;
2. Can we make the thread scheduling in the parallel testing deterministic like
   in the
   [*PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
   paper? Perhaps this is easier in other programming languages, e.g. via Rust's
   [tokio
   library](https://risingwave.com/blog/deterministic-simulation-a-new-era-of-distributed-system-testing/)
   or via multicore OCaml's effect handlers?
3. When there's a mismatch between the fake and the real system in the
   sequential case, we get a nice trace of what commands and state changes lead
   to the mismatch. In the concurrent case we didn't implement any such
   visualisation, although it would be useful to do so. The sequential trace can
   also be improved by for example showing the diffs of the state changes rather
   than repeating the whole state for each command;
4. In our examples we've mostly used a uniform distribution of commands, for our
   simple examples this is fine, but for more complicated examples we might need
   better random generation strategies. Here are a few papers on this topic that
   might be useful:
     * [*Swarm testing*](https://users.cs.utah.edu/~regehr/papers/swarm12.pdf) (2012);
     * [*Generating Good Generators for Inductive
       Relations*](https://dl.acm.org/doi/10.1145/3158133) (POPL 2018);
     * [*Beginner’s Luck: A Language for Property-Based
       Generators*](https://lemonidas.github.io/pdf/Luck.pdf) (POPL 2017).
5. We can also imagine not only using randomness for generating commands, see
   for example [*Coverage Guided, Property Based
   Testing*](https://dl.acm.org/doi/10.1145/3360607) (OOPSLA 2019) or
   [*Combinatorial Property-Based Testing: Do Judge a Test by its
   Cover*](https://link.springer.com/chapter/10.1007/978-3-030-72019-3_10)
   (ESOP, 2021) for two alternative generation strategies;
6. Stateful and parallel testing together with reusing the fakes to test bigger
   systems in a compositional way as covered in this post is enough for testing
   a lot of systems. The correctness of distributed systems however relies on
   the system functioning correctly even in the presence of failures. For this
   we need to introduce fault-injection. Luckily fakes make fault-injection
   easier. Recall how for the fakes we stored the state in a mutable variable,
   if we want to add fault-injection we can simply introduce yet another mutable
   variable which keeps track if the fault is enabled or not, and if it's then
   we throw some error rather than returning. Linearisability checking
   concurrent histories that contain timeouts involves more work than we've done
   in this post however. Essentially any operation that times out must be
   considered concurrent with all following operations. See the original
   [paper](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf) on
   linearisability and Jepsen's
   [Knossos](https://aphyr.com/posts/309-knossos-redis-and-linearizability)
   checker for more details on this topic;
7. The more operations that are concurrent at the same time the more work the
   linearisability checker has to do. One technique that can reduce the amount
   of work for the checker is partial order reduction. During concurrent
   execution sometimes we can commute two operations without changing the
   outcome, e.g. the interleaving of `Write "a" 1` and `Write "b" 2` doesn't
   matter, they all end up the same state. We can exploit this fact to check less
   histories;
8. The properties we've covered in this post are so called safety properties,
   i.e. "something bad will not happen". There's another class of properties
   called liveness and fairness properties, i.e. "something good will eventually
   happen". An example of liveness and fairness is that "a request made by a
   client will eventually be served". It would be interesting to see what's
   needed to support liveness and fairness. See
   [TLA+](https://lamport.azurewebsites.net/tla/tla.html) and the Haskell
   library
   [`quickcheck-dynamic`](https://hackage.haskell.org/package/quickcheck-dynamic);
   for more on this topic.
9. We've discussed how we can hide for example the file system behind an
   interface and use the fake implementation of that interface during testing.
   If one applies this technique to all non-deterministic aspects of a system
   (time, networking, randomness, etc) then one can run the system in a "fake
   world", this is also known as [simulation
   testing](https://youtube.com/watch?v=4fFDFbi3toc). How to carve out such
   interfaces in a way that makes it easy to do fault-injection and at the same
   time minimise the potential mismatch between the fake and the real
   implementation of said interfaces is a challenge.
