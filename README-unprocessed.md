# The sad state of property-based testing libraries

*Work in progress, please don't share, but do get involved!*

Property-based testing is a rare example of academic research that has made it
to the mainstream in less than 30 years. Under the slogan "don't write tests,
generate them" property-based testing has gained support from a diverse group of
programming language communities. In fact, the Wikipedia page of the original
property-basted testing Haskell library,
[QuickCheck](https://en.wikipedia.org/wiki/QuickCheck), lists 57
reimplementations in other languages.

In this post I'd like to survey the most popular property-based testing
implementations and compare them with what used to be the state-of-the-art
fifteen years ago (2009). As the title already gives away, most of the libraries
do not offer their users the most advanced property-based testing features. In
order to best explain what's missing and why I think we ended up in this
situation, let me start by telling the brief history of property-based testing.

## The history of property-based testing

In Gothenburg, Sweden's second most populated city, there's a university called
Chalmers. At the computer science department of Chalmers there are several
research groups, two of which are particularly relevant to our story -- the
*Functional Programming* group and *Programming Logic* group. I'll let you guess
what the former group's main interest is. The latter group's mostly concerned
with a branch of functional programming where the type system is sufficiently
expressive that it allows for formal specifications of programs, sometimes
called dependently typed programming or type theory. Agda is an example of a
Haskell-like dependently typed programming language, that also happens to be
mainly developed by the Programming Logic group. Given the overlap of interest
and proximity, researchers at the department are sometimes part of both groups
or at least visit each others research seminars from time to time.

John Hughes is a long-time member of the Functional Programming group, who's
also well aware of the research on dependently typed programming going on in the
Programming Logic group. One day in the late nineties, after having worked hard
on finishing something important on time, John found himself having a week
"off". So, just for fun, he started experimenting with the idea of testing if a
program respects a formal specification.

Typically in dependently typed programming you use the types to write the
specification and then the program that implements that type is the formal proof
that the program is correct. For example, let's say you've implemented a list
sorting function, the specification typically then is that the output of the
sorting function is ordered, i.e. for any index $i$ in your output list the
element at that index must be smaller or equal to the element at index $i + 1$.
Formally proving that a program is correct with respect to a specification is
often as much work as writing the program in the first place, so merely testing
it can often be a sweet spot where you get some confidence that the
specification is correct, without having to do the proving work. For example in
the sorting example you can simply generate a random input list and then compare
the output of your sorting function with the one in the standard library (which
is likely to be correct). As programs get more complicated the ratio of effort
saved by merely testing, as opposed to proving, increases. In fact for bigger
programs the effort involved in proving correctness is simply too high for it to
be practical (this is an active area of research). Given all this, I hope that
you can start to see why this idea excited John.

While John was working on this idea, Koen Claessen, another member of the
Functional Programming group, [stuck his
head](https://youtu.be/x4BNj7mVTkw?t=289) into John's office and asked what he
was doing. Koen got excited as well and came back the next day with his improved
version of John's code. There was some things that Koen hadn't thought about, so
John iterated on his code and so it went back and forth for a week until the
first implementation of property-based testing was written and not long
afterwards they published the paper [*QuickCheck: A Lightweight Tool for Random
Testing of Haskell
Programs*](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)
(ICFP 2000). I think it's worth stressing the *lightweight tool* part from the
paper's title, the complete source code for the [first
version](https://github.com/Rewbert/quickcheck-v1) of the library is included in
the appendix of the paper and it's about 300 lines of code.

Haskell and dependently typed programming languages, like Agda, are pure
functional programming languages, meaning that it's possible at the type-level
to distinguish whether a function has side-effects or not. Proofs about
functions in Agda, and similar languages, are almost always only dealing with
pure functions. Probably as a result of this, the first version of QuickCheck
can only test pure functions. This shortcoming was rectified in the follow up
paper [*Testing monadic code with
QuickCheck*](https://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps) (2002) by
the same authors. It's an important extension as it allows us to reason about
functions that use mutable state, file I/O and networking, etc. It also lays the
foundation for being able to test concurrent programs, as we shall see below.

Around the same time as the second paper was published (2002), John was applying
for a major grant at the Swedish Strategic Research Foundation. A part of the
application process involved pitching in front of a panel of people from
industry. Some person from [Ericsson](https://en.wikipedia.org/wiki/Ericsson)
was on the panel and they were interested in QuickCheck. There was also a serial
entrepreneur on the panel and she encouraged John to start a company, and the
Ericsson person agreed to be a first customer, and so Quviq AB was founded in
2006[^1] by John and Thomas Arts (perhaps somewhat surprisingly, Koen was never
involved in the company).

The first project at Ericsson that Quviq helped out testing was written in
Erlang. Unlike Haskell, Erlang is not a pure functional programming language and
on top of that there's concurrency everywhere. So even the second, monadic,
version of QuickCheck didn't turn out to be ergonomic enough for the job. This
is what motivated the closed source Quviq QuickCheck version written in Erlang,
first mentioned in the paper [*Testing telecoms software with Quviq
QuickCheck*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=b268715b8c0bcebe53db857aa2d7a95fbb5c5dbf)
(2006). The main features of the closed source version that, as we shall see,
are still not found in many open source versions are:

  1. Sequential *stateful* property-based testing using a state machine model;
  2. *Parallel* testing with race condition detection by reusing the sequential
    state machine model.

We shall describe how these features work in detail later. For now let's just
note that *stateful* testing in its current form was first mentioned in
[*QuickCheck testing for fun and
profit*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
(2007). This paper also mentions that it took John four iterations to get the
stateful testing design right, so while the 2006 paper already does mention
stateful testing it's likely containing one of those earlier iteration of it.

While the 2007 paper also mentions *parallel* testing via traces and
interleavings, it's vague on details. It's only later in [*Finding Race
Conditions in Erlang with QuickCheck and
PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
(ICFP 2009) that parallel testing is described in detail including a reference
to [*Linearizability: a correctness condition for concurrent
objects*](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf) by Herlihy and
Wing (1990) which is the main technique behind it.

I'd like to stress that no Quviq QuickCheck library code is ever shared in any
of these papers, they only contain the library APIs (which are public) and test
examples implemented using said APIs.

After that most papers are experience reports of applying Quviq QuickCheck at
different companies, e.g. *Testing A Database for Race Conditions with
QuickCheck* (2011), [*Testing the hard stuff and staying
sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
(2014), *Testing AUTOSAR software with QuickCheck* (2015), *Mysteries of
Dropbox: Property-Based Testing of a Distributed Synchronization Service*
(2016).

Sometimes various minor extensions to stateful and parallel testings are needed
in order to test some particular piece of software, e.g. C FFI bindings in the
case of AUTOSAR or eventual consistency in the case of Dropbox, but by and large
the stateful and parallel testing features remain the same.

## A survey of property-based testing libraries

As we've seen above, the current state-of-the-art when it comes to
property-based testing is *stateful* testing via a state machine model and
reusing the same sequential state machine model combined with linearisability to
achieve *parallel* testing.

Next let's survey the most commonly used property-based testing libraries to see
how well supported these two testing features are. Let me be clear up front that
I've not used all of these libraries. My understanding comes from reading the
documentation, issue tracker and sometimes source code.

To my best knowledge, as of June 2024, the following table summarises the
situation. Please open an
[issue](https://github.com/stevana/stateful-pbt-with-fakes/issues), PR, or get
in [touch](https://stevana.github.io/about.html) if you see a mistake or an
important omission.

| Library | Language | Stateful | Parallel | Notes |
| :---    | :---     | :---:    | :---:    | :---  |
| Eris | PHP | ☐ | ☐ | |
| FsCheck | F# | ☒ | ☐ | Has experimental [stateful testing](https://fscheck.github.io/FsCheck//StatefulTestingNew.html). An [issue](https://github.com/fscheck/FsCheck/issues/214) to add parallel support has been open since 2016. |
| Gopter | Go | ☒ | ☐ | The README says "No parallel commands ... yet?" and there's an open [issue](https://github.com/leanovate/gopter/issues/20) from 2017. |
| Hedgehog | Haskell | ☒ | ☒ | Has parallel support, but the implementation has [issues](https://github.com/hedgehogqa/haskell-hedgehog/issues/104). |
| Hypothesis | Python | ☒ | ☐ | |
| PropEr | Erlang | ☒ | ☒ | First open source library to support both? |
| QuickCheck | Haskell | ☐ | ☐ | There's an open [issue](https://github.com/nick8325/quickcheck/issues/139) to add stateful testing since 2016. |
| QuickTheories | Java | ☒ | ☐ | Has [experimental](https://github.com/quicktheories/QuickTheories/issues/42) for stateful testing, there's also some parallel testing, but it's inefficient and restrictive compared to QuviQ's Erlang version of QuickCheck. From the [source code](https://github.com/quicktheories/QuickTheories/blob/a963eded0604ab9fe1950611a64807851d790c1c/core/src/main/java/org/quicktheories/core/stateful/Parallel.java#L35): "Supplied commands will first be run in sequence and compared against the model, then run concurrently. All possible valid end states of the system will be calculated, then the actual end state compared to this. As the number of possible end states increases rapidly with the number of commands, command lists should usually be constrained to 10 or less." |
| Rapid | Go | ☒ | ☐ | |
| RapidCheck | C++ | ☒ | ☐ | There's an open [issue](https://github.com/emil-e/rapidcheck/issues/47) to add parallel support from 2015. |
| ScalaCheck | Scala | ☒ | ☐ | Has some support for parallel testing, but it's limited as can be witnessed by the fact that the two [examples](https://github.com/typelevel/scalacheck/tree/19af6eb656ba759980664e29ec6ae3e063021685/examples) of testing LevelDB and Redis both are sequential (`threadCount = 1`). |
| SwiftCheck | Swift | ☐ | ☐ | There's an open [issue](https://github.com/typelift/SwiftCheck/issues/149) to add stateful testing from 2016. |
| fast-check | TypeScript | ☒ | ☐ | Has [some support](https://fast-check.dev/docs/advanced/race-conditions/) for race condition checking, but it seems different from Quviq QuickCheck's parallel testing. In particular it doesn't seem to reuse the sequential state machine model nor use linearisability. |
| jetCheck | Java | ☒ | ☐ | From the source code "Represents an action with potential side effects, for single-threaded property-based testing of stateful systems.". |
| jsverify | JavaScript | ☐ | ☐ | There's an open [issue](https://github.com/jsverify/jsverify/issues/148) to add stateful testing from 2015. |
| lua-quickcheck | Lua | ☒ | ☐ | |
| propcheck | Elixir | ☒ | ☐ | There's an open [issue](https://github.com/alfert/propcheck/issues/148) to add parallel testing from 2020. |
| proptest | Rust | ☐ | ☐ | See proptest-state-machine. |
| proptest-state-machine | Rust | ☒ | ☐ | Documentation says "Currently, only sequential strategy is supported, but a concurrent strategy is planned to be added at later point.". |
| qcheck-stm | OCaml | ☒ | ☒ | |
| quickcheck | Prolog | ☐ | ☐ | |
| quickcheck | Rust | ☐ | ☐ | Issue to add stateful testing has been [closed](https://github.com/BurntSushi/quickcheck/issues/134). |
| quickcheck-state-machine | Haskell | ☒ | ☒ | Second open source library with parallel testing support? (I was [involved](https://github.com/nick8325/quickcheck/issues/139#issuecomment-272439099) in the development.) |
| rackcheck | Racket | ☐ | ☐ |  |
| rantly | Ruby | ☐ | ☐ | |
| test.check | Clojure | ☐ | ☐ | Someone has implemented stateful testing in a blog [post](http://blog.guillermowinkler.com/blog/2015/04/12/verifying-state-machine-behavior-using-test-dot-check/) though. |
| theft | C | ☐ | ☐ | |

## Analysis

By now I hope that I've managed to convince you that most property-based testing
libraries do not implement what used to be the state-of-the-art fifteen years
ago.

Many libraries lack stateful testing via state machines and most lack parallel
testing support. Often users of the libraries have opened tickets asking for
these features, but the tickets have stayed open for years without any progress.
Furthermore it's not clear to me whether all libraries that support stateful
testing can be generalised to parallel testing without a substantial redesign of
their APIs. I don't think there's a single example of a library to which
parallel testing was added later, rather than designed for from the start.

### Why are property-based testing libraries in such a sad state?

Here are three reasons I've heard from John:

1. The stateful and parallel testing features are not as useful as testing pure
   functions. This is what John told me when I asked him why these features
   haven't taken off in the context of Haskell (BobKonf 2017);

2. The state machine models that one needs to write for the stateful and
   parallel testing require a different way of thinking compared to normal
   testing. One can't merely give these tools to new users without also giving
   them proper training, John said in an
   [interview](https://youtu.be/x4BNj7mVTkw?t=898);

3. Open source didn't work, a closed source product and associated services
   [helps](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
   adoption:

   > "Thomas Arts and I have founded a start-up, Quviq AB, to develop and market
   > Quviq QuickCheck. Interestingly, this is the second implementation of
   > QuickCheck for Erlang. The first was presented at the Erlang User
   > Conference in 2003, and made available on the web. Despite enthusiasm at
   > the conference, it was never adopted in industry. We tried to give away the
   > technology, and it didn’t work! So now we are selling it, with considerably
   > more success. Of course, Quviq QuickCheck is no longer the same product
   > that was offered in 2003—it has been improved in many ways, adapted in the
   > light of customers’ experience, extended to be simpler to apply to
   > customers’ problems, and is available together with training courses and
   > consultancy. That is, we are putting a great deal of work into helping
   > customers adopt the technology. It was naive to expect that simply putting
   > source code on the web would suffice to make that happen, and it would also
   > be unreasonable to expect funding agencies to pay for all the work
   > involved. In that light, starting a company is a natural way for a
   > researcher to make an impact on industrial practice—and so far, at least,
   > it seems to be succeeding."

A cynic might argue that there's a conflict of interest between doing research
and education on one hand and running a company that sells licenses, training
and consulting on the other.

Let me be clear that I've the utmost respect for John, and I believe what he
says to be true and I believe he acts with the best intentions. Having said that
let me try to address John's points.

#### Stateful and parallel testing isn't as useful as pure testing

I think many people will agree that separating pure from side-effectful code is
good practice in any programming language, and I do agree with John that you can
get far by merely property-based testing those pure fragments.

However I also think that stateful and parallel testing is almost equally
important for many non-trivial software systems. Most systems in industry will
have some database, stateful protocol or use concurrent data structures, which
all benefit from the stateful and parallel testing features.

#### Stateful modelling requires training

Regarding formal specification requiring a special way of thinking and therefor
training, I believe this is a correct assessment. However I also believe that
this is already true for property-based testing of pure functions. A non-trained
user of pure property-based testing will likely test less interesting properties
than someone who's trained.

Given that John has written
[papers](https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf)
and given [talks](https://www.youtube.com/watch?v=NcJOiQlzlXQ) on the topic of
making property-based testing of pure functions more accessible to programmers,
one might wonder why we cannot do the same for stateful and parallel testing?

The experience reports, that we've mentioned above, usually contain some novelty
(which warrants publishing a new paper) rather than general advice which can be
done with the vanilla stateful and parallel testing features. Furthermore they
require buying a Quviq license in order to reproduce the results, a show stopper
for many people.

I think it's also worth stressing that stateful specifications are not
necessarily always more difficult than specifications for pure functions. For
example, to model a key-value store one can get quite far with the model being a
list of key-value pairs. In fact a simple model like that managed to find a 17
step (shrunk) counterexample in LevelDB to a known
[issue](https://github.com/google/leveldb/issues/50), within mere minutes. It
took weeks for Google to provide a fix, and then after running the property
again on the fixed code a new 31 step counterexample was found within minutes.
Turns out there was a bug in the background compaction process. The compaction
process improves read performance and reclaims disk space, which is important
for a key-value store, but interestingly it's not explicitly part of the model.
Joseph W Norton gave a
[talk](https://htmlpreview.github.io/?https://raw.githubusercontent.com/strangeloop/lambdajam2013/master/slides/Norton-QuickCheck.html)
at LambdaJam 2013 about it.

#### Closed source helps industry adoption

Regarding keeping the source closed helping with adoption, I think this is
perhaps the most controversial point that John makes.

If we try to see it from John's perspective, how else would an academic get
funding to work on tooling (which typically isn't recognised as doing
research), or feedback from industry? Surely, one cannot expect research funding
agencies to pay for this?

On the other hand one could ask why there isn't a requirement that published
research should be reproducible using open source tools (or at least tools that
are freely available to the public and other researchers)? Trying to replicate
the results from the Quviq QuickCheck papers (from 2006 and onward) without
buying a Quviq QuickCheck license, is almost impossible without a lot of reverse
engineering work.

I suppose one could argue that one could have built a business around an open
source tool, only charging for the training and consulting, but given how broken
open source is today, unless you are a big company (which takes more than it
gives back), it's definitely not clear that it would have worked (and it was
probably even worse back in 2006).

Even if John is right and that keeping it closed source has helped adoption in
industry, I think it's by now fair to say it has not helped open source
adoption. Or perhaps another way to look at it, it's unlikely that a company
that pays for a license in Erlang would then go and port the library in another
language.

### What can we do about it?

I think there's at least two things worth trying.

1. Provide a short open source implementation of stateful and parallel
   property-based testing, analogous to the original ~300 lines of code
   QuickCheck implementation.

   Perhaps part of the original QuickCheck library's success in spreading to so
   many other languages can be attributed to the fact that its small
   implementation and that it is part of the original paper?

2. Try to make the formal specification part easier, so that we don't need to
   train developers (as much).

   Perhaps we can avoid state machines as basis for specifications and instead
   reuse concepts that programmers are already familiar with from their current
   testing activities, e.g. mocking and test doubles more generally?

## Synthesis

In order to test the above hypothesis, I'd like to spend the rest of this post
as follows:

  1. Show how one can implement stateful and parallel property-based testing in
     about 400 lines of code (similar to the size of the original QuickCheck
     implementation);

  2. Make specifications simpler by using
     [fakes](https://martinfowler.com/bliki/TestDouble.html) rather than state
     machines.

Before we get started with stateful testing, let's first recap how
property-based testing of pure functions works.

### Pure property-based testing recap

It's considered good practice to test new functions or functionality, to make
sure it does what we want. For example, imagine we've written a linked-list
reversal function called `reverse`, then it might be sensible to test it against
a couple of lists such as the empty list and, say, the three element list `[1,
2, 3]`.

How does one choose which example inputs to test against though? Typically one
wants to choose corner cases, such as the empty list, that perhaps were
overlooked during the implementation. It's difficult to think of corner cases
that you might have overlooked (because if you can then you probably wouldn't
have overlooked them in the first place)! This is where generating random
inputs, a key feature of property-based testing, comes in. The idea being that
random inputs will eventually hit corner cases.

When we manually pick inputs for our tests, like `[1, 2, 3]`, we know what the
output should be and so we can make the appropriate assertion, i.e. `reverse [1,
2, 3] == [3, 2, 1]`. When we generate random inputs we don't always know what
the output should be. This is where writing properties that relate the output to
the input somehow comes in. For example, while we don't know what the output of
reversing an arbitrary list is, we do know that reversing it twice will give
back the input. This is how we can express this property in QuickCheck:

```
>>> quickCheck (\(xs :: [Int]) -> reverse (reverse xs) == xs)
+++ OK, passed 100 tests.
```

By default 100 tests get generated, but that can be adjusted:

```
>>> quickCheck (withMaxSuccess 5 (\(xs :: [Int]) -> reverse (reverse xs) == xs))
+++ OK, passed 5 tests.
```

We can see what test get generated using `verboseCheck`:

```haskell
>>> verboseCheck (withMaxSuccess 5 (\(xs :: [Int]) -> reverse (reverse xs) == xs))
Passed:
[]

Passed:
[1]

Passed:
[-2]

Passed:
[2]

Passed:
[-4,-2,-2,3]

+++ OK, passed 5 tests.
```

Or by using `sample` on the appropriate `Gen`erator. In this case we are
generating lists of integers, hence the `Gen [Int]` type annotation:

```haskell
>>> sample (arbitrary :: Gen [Int])
[]
[]
[-2,2,3,-2]
[-4]
[4,6,-2,-6,-1]
[1,7,5,-8,1]
[-11,4]
[3,-1,11]
[]
[-3,17,-14,-1,17,18,-8,-9,-13,-7]
[6,19,6,9,-15,-6,-19]
```

The list and integer generators are provided by the library and I hope you agree
that these seem like sensible arbitrary lists to use in our tests.

Next let's have a look at when a property fails. For example this is what
happens if we try to test that the output of reversing a list is the input list:

```
>>> quickCheck (\(xs :: [Int]) -> reverse xs == xs)
*** Failed! Falsified (after 3 tests and 2 shrinks):
[0,1]
```

We see that after 3 tests a test case was generated that failed, the input got
shrunk twice and the minimal counterexample `[0, 1]` is presented. Notice that
we do need a list that is at least of length two, because any shorter list will
reverse to itself.

As pointed out earlier, coming up with these properties is by no means obvious.
There are however a few patterns that come up over and over again. With
`reverse` we saw an example of an involutory function, i.e. `f (f x) == x`, here
are a few other examples:

- Inverses, e.g. `\(i :: Input) -> deserialise (serialise i) == i`;
- Idempotency, e.g. `\(xs :: [Int]) -> sort (sort xs) == sort xs`;
- Associativity, e.g. `\(i j k :: Int) -> (i + j) + k == i + (j + k)`;
- Axioms of abstract data types, e.g. `\(x :: Int)(xs :: [Int]) -> member x
  (insert x xs) && not (member x (remove x xs))`;
- Metamorphic properties, e.g. `\(g :: Graph)(m n :: Node) -> shortestPath g m n ==
  shortestPath g n m`.

Readers familiar with discrete math might recognise some of the above.

### Stateful property-based testing

In the pure property-based testing case, that we just looked at, the picture
of the test setup looks a bit like this:


```
         +-----+
      i  |     |  o
    ----->  f  +---->
         |     |
         +-----+
```

Where `i` is the input we generate, `f` is the function we are applying the
generated input to to produce the output `o`. In the case of the `reverse`
example, from before, `i` and `o` are of type list of integers (`[Int]`), `f` is
`reverse . reverse` and the property that we check for every generated input is
that input is equal to the output.

Next let's contrast this picture with how the test setup looks when we are
testing a stateful component. A simple example of a stateful component is a
counter with an `incr`ement operation which increment the counter and returns
the old count.

Unlike in the pure case, the same input will not give the same output. For
example the first time we do `incr` we get back `0` (if we start counting from
zero) while the second time we do `incr` we get `1`. A database or a file system
are two other examples of stateful components, where the history of previous
inputs affects the output of the next input.

In the stateful case, the picture looks more like this:

```
    +------+     +------+     +------+
    |      | i1  |      | i2  |      |
    |  s0  +----->  s1  +----->  s2  | ...
    |      |     |      |     |      |
    +------+     +--+---+     +--+---+
                    |            |
                    | o1         | o2
                    v            v

    ---------------------------------> time
```

Where `s` is the state, `i` is an input (e.g. `incr`) and `o` is an output.
Notice how the state evolves over time and depends on the history of inputs.

In the pure case each test case is a single input, in the stateful case we need
a sequence of inputs in order to test how the system changes over time. In the
pure case our our properties were relations on the input and output, i.e. `R : i
-> o -> Bool`. In the stateful case our properties would need to be generalised
to `R' : [i] -> [o] -> Bool` to account for how the state changes over time.
Writing such properties is cumbersome, an alternative is to account for the
state explicitly by means of some kind of model.

This model could be a state machine of type `m -> i -> (m, o)`, i.e. a function
from the old model and an input to the next model and the output. From this we
can derive a property that for each input checks if the outputs of the stateful
component agrees with the output of the state machine:

```
   +------+     +------+     +------+
   |      | i1  |      | i2  |      |
   |  s0  +----->  s1  +----->  s2  | ...
   |      |     |      |     |      |
   +------+     +--++--+     +--++--+
                   ||           ||
                   ||o1         || o2
                   ||           ||
   +------+     +--++--+     +--++--+
   |      | i1  |      | i2  |      |
   |  m0  +----->  m1  +----->  m2  | ...
   |      |     |      |     |      |
   +------+     +------+     +------+
```

In case the outputs disagree we shrink the sequence of inputs and try to present
the smallest counterexample, as in the pure case.

Let's make things more concrete with some actual code that we can run.

#### Example: counter

All examples, in the rest of this post, will have three parts:

  1. The software under test;
  2. The model that the software under test gets tested against;
  3. The generated tests and output from running them.

The first part is independent of the stateful testing library we are building.
The second part is hooking up the first part to the library by implementing an
interface (type class). We'll look at the definition of the type class after the
example. The final part is how to write the actual property and interpret the
output from running them.

##### Software under test

This is how you can implement a counter using a global mutable variable in
Haskell:

```haskell
gLOBAL_COUNTER :: IORef Int
gLOBAL_COUNTER = unsafePerformIO (newIORef 0)
{-# NOINLINE gLOBAL_COUNTER #-}

incr :: IO ()
incr = do
  n <- readIORef gLOBAL_COUNTER
  writeIORef gLOBAL_COUNTER (n + 1)

get :: IO Int
get = readIORef gLOBAL_COUNTER
```

Notice that here `incr` doesn't return the old value, like above, and instead we
have a separate operation `get` which returns the current value of the counter.

##### Model

To model our counter we'll use an integer.

```haskell
newtype Counter = Counter Int
  deriving (Eq, Show)

-- We'll come back to the definition of the `StateModel` type class after this
-- example.
instance StateModel Counter where

  -- We start counting from zero.
  initialState :: Counter
  initialState = Counter 0

  -- The commands correspond to the names of the functions that operate on the
  -- global counter.
  data Command Counter r
    = Incr
    | Get
    deriving (Show, Functor)

  -- The responses correspond to the return types of each function. By
  -- convention we'll add a underscore suffix to a response of the corresponding
  -- command.
  data Response Counter r
    = Incr_ ()
    | Get_ Int
    deriving (Eq, Show, Functor, Foldable)

  -- The state machine takes a command and the model of the counter and returns
  -- a new model and a response. We'll come back to the role of the `Either` later.
  runFake :: Command Counter r -> Counter -> Either Void (Counter, Response Counter r)
  runFake Incr  (Counter n) = return (Counter (n + 1), Incr_ ())
  runFake Get m@(Counter n) = return (m, Get_ n)

  -- We also need to explain which part of the counter API each command
  -- corresponds to.
  runReal :: Command Counter r -> IO (Response Counter r)
  runReal Get  = Get_  <$> get
  runReal Incr = Incr_ <$> incr

  -- We'll generate increments and reads of the counter with equal probability.
  -- Notice that we only need to explain how to generate a single command, the
  -- library will use this to generate sequences of commands as we'll see later.
  generateCommand :: Counter -> Gen (Command Counter r)
  generateCommand _s = elements [Incr, Get]
```

A common complaint is that the model (`Counter` and `runFake`) is as big as the
implementation itself. This is true, because it's an example. In reality the
model will often be many orders of magnitude smaller. This is due to the fact
that the model, unlike the real implementation, doesn't need to persisting to
disk, communicating over the network, or perform various time or space
optimisations. Recall the LevelDB example from above.

##### Tests

The tests, or property, can now be written as follows.

```haskell
prop_counter :: Commands Counter -> Property
prop_counter cmds = monadicIO $ do
  runCommands cmds
  run reset
  assert True

reset :: IO ()
reset = writeIORef gLOBAL_COUNTER 0
```

To run them, we can load the module and type `quickCheck prop_counter` in the
REPL, which gives us an output like:

```
+++ OK, passed 100 tests:
89% Get
85% Incr

Commands (2151 in total):
52.02% Get
47.98% Incr
```

Where the first group of percentages tell us the proportion of tests that
contained the get and increment command respectively, and the second group of
percentages tell us the proportion of get and increment commands out of all
commands generated. Note that the first group doesn't add up to 100%, because
most tests will contain both commands, whereas the second group does. The reason
the second group is almost 50-50 is because in the generator we generate both
commands with equal probability.

Another thing to note is that we need to `reset` the counter between tests,
otherwise the global counter will have the value from the last test while the
model always starts from zero and we get a mismatch.

To make things a bit more interesting, let's introduce a bug into our counter
and see if the tests can find it. Let's make it so that if the counter has the
value of 42, then it won't increment properly.

```haskell
incr42Bug :: IO ()
incr42Bug = do
  n <- readIORef gLOBAL_COUNTER
  let n' = if n == 42
           then n -- BUG
           else n + 1
  writeIORef gLOBAL_COUNTER n'
```

We also need to change the `runReal` function to use our buggy increment as
follows.

```diff
- runReal Incr = Incr_ <$> incr
+ runReal Incr = Incr_ <$> incr42Bug
```

When we run the property now, we'll see something like the following output.

```
*** Failed! Assertion failed (after 66 tests and 29 shrinks):
    Commands [Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Incr,Get]
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Incr --> Incr_ ()
    Get --> Get_ 42

    Expected: Get_ 43
    Got: Get_ 42
```

Notice that this is indeed the smallest counterexample and how it took 66
randomly generated test cases to find the sequence of inputs that triggered the
bug and then 29 shrink steps for QuickCheck to minimise it.

#### Stateful library implementation

In the example above we implemented the `StateModel` interface (or type class),
next we'll have a look at the definition of this interface and the testing
functionality we can derive by programming against the interface.

##### Stateful testing interface

Let me give you the full definition of the interface and then I'll explain it in
words afterwards.

```haskell
class ( ...
```
```{.haskell include=src/Stateful.hs snippet=StateModel}
```

The interface is parametrised by a `state` type that the user needs to define
before instantiating the interface. In the counter example `state` is `newtype
Counter = Counter Int`. The user needs to provide an `initialState :: state`
from which we'll start generating commands and executing the model, in the
counter case this is `Counter 0`.

As part of the instantiating the user also needs to specify a type of `Command`s
and `Response`s, these were the `Incr` and `Get` operations of the counter and
their response types respectively.

In addition there's also three optional types, that we've not needed in the
counter example. The first is references, these are used to refer back to
previously created resources. For example if we open a file handle on a
POSIX-like file system, then later commands need to be able to refer to that file
handle when wanting to write or read from it. The second datatype is
`PreconditionFailure`, which is used to give a nice error message when a command
is executed in a disallowed state. For example if we try to read from a file
handle that has been closed. The third data type is `CommandMonad` which let's
us use a different monad than `IO` for executing our commands in. After we've
finished with the interface definition we'll come back to more examples where
we'll use these optional types, hopefully these examples will help make things
more concrete.

We've already seen that the user needs to provide a way to generate single
command, the only thing worth mentioning is that in case our commands contain
references then during the generation phase we only deal with `Var`s of
references, where `data Var a = Var Int`. The reason for this is that we cannot
generate, for example, real file handles (only the operating system can), so
instead we generate symbolic references which are just `Int`s. Think of these as
placeholders for which real references will be substituted in once the real
references are created during execution.

Shrinking of individual commands is optional and disabled by default, but as
we've seen this doesn't exclude the sequence of commands to be shrunk. We'll
shall see shortly how that is done in detail.

Next up we got `runFake` and `runReal` which executes a command against the
`state` model and the real system respectively. Notice how `runFake` can fail
with a `PreconditionFailure`, whereas `runReal` is always expected to succeed
(because if a command fails the precondition check, then it won't get generated
and hence never reach `runReal`). Another difference is that `runFake` uses
symbolic references, while `runReal` deals with real references. We'll shortly
see how this substitution of references works.

Lastly we have two optional functions related to keeping statistics of generated
test cases, which is useful for coverage reporting among other things. We'll
come back to how these can be used as we look at more examples after we've
defined our stateful property-based testing library.

##### Generating and shrinking

Once we have our interface we can start writing functions against the interface.
These functions are what the user gets once they implement the interface. In
this section we'll have a look at generation of sequences of `Commands`, which
will be the inputs for our tests, and how to shrink said inputs to produce a
minimal counterexample.

Let's start by defining `Commands`, notice that they use symbolic references
(i.e. `Var (Reference state)`):

```{.haskell include=src/Stateful.hs snippet=Commands}
```

As mentioned above, when we generate commands we cannot generate real
references, e.g. file handles, thus `Var (Reference state)` is used which is
isomorphic to just an `Int`.

Sometimes it's convenient to split up `runFake` into two parts, the first checks
if the command is allowed in the current state, i.e. the precondition holds:

```{.haskell include=src/Stateful.hs snippet=precondition}
```

And the second part advances the state:

```{.haskell include=src/Stateful.hs snippet=nextState}
```

We assume that we'll only ever look at the `nextState` when the `precondition`
holds.

Using these two functions we can implement QuickCheck's `Arbitrary` type class
for `Commands` which let's us generate and shrink `Commands`:

```{.haskell include=src/Stateful.hs snippet=Arbitrary}
```

Notice how after shrinking we prune away all commands that don't pass the
precondition or that are out of scope with respect to symbolic references.

The intuition here is that as we remove commands from the originally generated
`Commands` (which all pass their preconditions), we might have broken some
preconditions and pruning simply removes the commands which we made invalid in
the process of shrinking. Similarly we can have a command that creates a
reference that later commands then depend on, if we during shrinking remove the
command that created the reference then we must also remove the commands that
depend on the reference.

##### Running and assertion checking

Once we've generated `Commands` we need to execute them against the model and
the real system using `runFake` and `runReal`. In the process of doing so
`runReal` will produce `Reference`s that later commands might use, so we also
need to substitute symbolic references for real references. This, together with
coverage statistics bookkeeping, is done in the `runCommands` function:

```{.haskell include=src/Stateful.hs snippet=runCommands}
```

Where `Env` is defined as follows.

```{.haskell include=src/Stateful.hs snippet=Env}
```

That's all the pieces we need to implement that `Counter` example that we saw
above, plus some new constructs to deal with precondition failures and
references.

Next let's have a look at an example where we need preconditions and references.

#### Example: circular buffer

This example is taken from John's paper [*Testing the hard stuff and staying
sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
(2014).

##### Software under test

The implementation is written in C and uses two indices which keep track of the
front and back of the queue, this allows us to implement the queue in a circular
fashion. I've copied the C code straight from the paper. In order to test it
from Haskell, we'll use Haskell's foreign function interface.

```c
typedef struct queue {
  int *buf;
  int inp, outp, size;
} Queue;

Queue *new(int n) {
  int *buff = malloc(n*sizeof(int));
  Queue q = {buff,0,0,n};
  Queue *qptr = malloc(sizeof(Queue));
  *qptr = q;
  return qptr;
}

void put(Queue *q, int n) {
  q->buf[q->inp] = n;
  q->inp = (q->inp + 1) % q->size;
}

int get(Queue *q) {
  int ans = q->buf[q->outp];
  q->outp = (q->outp + 1) % q->size;
  return ans;
}

int size(Queue *q) {
  return (q->inp - q->outp) % q->size;
}
```

Notice that the C code doesn't do any error checking, e.g. if we `get` from an
empty queue then we'll get back uninitialised memory.

##### Model

The circular buffer implementation is very efficient, because it reuses the
allocated memory as we go around in circles, but it's not obviously correct.

To model queues we'll use a more straight forward non-circular implementation.
This is less efficient (doesn't matter as it's merely used during testing), but
hopefully more obviously correct.

```haskell
data FQueue = FQueue
  { fqElems :: [Int]
  , fqSize  :: Int
  }
```

In the `Counter` example above we only had one counter, so the model was merely
a single integer. In this example, because of `new` returning a queue, we need
to be able to model arbitrary many queues. We can do this using symbolic
references (`data Var a = Var Int`) as follows:

```haskell
type State = Map (Var Queue) FQueue

emptyState :: State
emptyState = Map.empty
```

Where `Queue` is the Haskell data type that corresponds to the C `Queue` and the
`Var a` data type is provided by the library and is a symbolic reference to `a`
(just an `Int`eger). The idea being that in the model we don't have access to
real `Queue`s, merely symbolic references to them. This might seem a bit
strange, but I hope that it will become more clear when we model `new`.

```haskell
type FakeOp a = State -> Either Err (State, a)

fNew :: Int -> FakeOp (Var Queue)
fNew sz s =
  let
    v  = Var (Map.size s)
    s' = Map.insert v (FQueue [] sz) s
  in
    return (s', v)
```

As we have access to the state when defining our model, we can create new unique
symbolic references by simply counting how many symbolic references we've
created previously (using `Map.size`)[^2].

As we said before, in the C code we don't do any error checking. In the model we
do check that, for example, the queue is non-empty before we `fGet` an item.
These are our preconditions.

```haskell
data Err
  = QueueDoesNotExist
  | QueueIsEmpty
  deriving (Eq, Show)


fPut :: Var Queue -> Int -> FakeOp ()
fPut q i s
  | q `Map.notMember` s = Left QueueDoesNotExist
  | otherwise = return (Map.adjust (\fq -> fq { fqElems = fqElems fq ++ [i] }) q s, ())

fGet :: Var Queue -> FakeOp Int
fGet q s
  | q `Map.notMember` s = Left QueueDoesNotExist
  | otherwise = case fqElems (s Map.! q) of
      []     -> Left QueueIsEmpty
      i : is -> return (Map.adjust (\fq -> fq { fqElems = is }) q s, i)

fSize :: Var Queue -> FakeOp Int
fSize q s
  | q `Map.notMember` s = Left QueueDoesNotExist
  | otherwise           = return (s, length (fqElems (s Map.! q)))
```

Recall that we won't generate a get operation unless the precondition holds in
the state that we are currently in, i.e. we will never generate gets if the
queue is empty and thus we'll never execute the C code for `get` which gives
back uninitialised memory.

Having defined our model the interface implementation is almost mechanical.

```{.haskell include=src/Example/Queue/Test.hs snippet=QueueStateModel}
```

The only new thing worth paying attention to is the `q` in `Command` and
`Response`, which is parametrised so that it works for both symbolic and real
references. The `Functor` instance lets us do substitution, while `Foldable`
lets us extract all new references from a response, so that we can substitute
them in later `Command`s.

##### Testing

Having implemented the interface, we can write our property as follows.

```{.haskell include=src/Example/Queue/Test.hs snippet=prop_queue}
```

When we run it, using `quickCheck prop_queue`, we get the following error.

```
   *** Failed! Assertion failed (after 7 tests and 5 shrinks):
    Commands {unCommands = [New 1,Put (Var 0) 0,Put (Var 0) 1,Get (Var 0)]}
    New 1 --> New_ (Queue 0x00000000016e9010)
    Put (Var 0) 0 --> Put_ ()
    Put (Var 0) 1 --> Put_ ()
    Get (Var 0) --> Get_ 1
    Expected: Get_ 0
    Got: Get_ 1
```

So we create a new queue of size `1`, put two items (`0` and `1`) into it, and
finally we read a value from the queue and this is where the assertion fails. Or
model returns `0`, because it's a FIFO queue, but the C code returns `1`. The
reason for this is that in the C code there's no error checking, so writing a
value to a full queue simply overwrites the oldest value. So there's actually
nothing wrong with the implementation, but rather the model is wrong. We've
forgotten a precondition:

```diff
 data Err
   = QueueDoesNotExist
   | QueueIsEmpty
+  | QueueIsFull
```

```diff
fPut :: Var Queue -> Int -> State -> Either Err (State, ())
fPut q i s
  | q `Map.notMember` s = Left QueueDoesNotExist
+ | length (fqElems (s Map.! q)) >= fqSize (s Map.! q) = Left QueueIsFull
  | otherwise = return (Map.adjust (\fq -> fq { fqElems = fqElems fq ++ [i] }) q s, ())
```

We can add the counterexample that we got as a regression test to our test suite
as follows:

```{.haskell include=src/Example/Queue/Test.hs snippet=unit_queueFull}
```

Notice that we can basically copy-paste `cmds` from QuickCheck's output, but
I've done some formatting here to make it more readable.

After fixing the precondition for `fPut`, `unit_queueFull` fails as follows:

```
+++ OK, failed as expected. Assertion failed (after 1 test):
New 1 --> New_ (Queue 0x00000000006f6d20)
Put (Var 0) 1 --> Put_ ()
Preconditon failed: QueueIsFull
```

When we rerun `quickCheck prop_queue` we will not generate this example again,
because all preconditions need to hold, and the property passes:

```
>>> quickCheck prop_queue
+++ OK, passed 100 tests:
95% New
86% Put
67% Get

Commands (2497 in total):
44.13% New
41.25% Put
14.62% Get
```

However as we can see in the output there's no coverage for `Size`! The reason
for this is because we've forgot to add it to our generator:

```diff
  generateCommand s
    | Map.null s = New . getPositive <$> arbitrary
    | otherwise  = oneof
      [ New . getPositive <$> arbitrary
      , Put  <$> arbitraryQueue <*> arbitrary
      , Get  <$> arbitraryQueue
+     , Size <$> arbitraryQueue
      ]
```

After adding it and rerunning the property, we get the following error:

```
>>> quickCheck prop_queue
*** Failed! Assertion failed (after 25 tests and 8 shrinks):
Commands {unCommands = [New 1,Put (Var 0) 0,Size (Var 0)]}
New 1 --> New_ (Queue 0x0000000001444220)
Put (Var 0) 0 --> Put_ ()
Size (Var 0) --> Size_ 0
Expected: Size_ 1
Got: Size_ 0
```

Size should return how many items are in the queue, so after we put one item
into a queue we expect it to return `1`, but in the above counterexample it
returns `0`.

To understand why this happens we have to look at how `put` and `size` are implemented:

```c
void put(Queue *q, int n) {
  q->buf[q->inp] = n;
  q->inp = (q->inp + 1) % q->size;
}

int size(Queue *q) {
  return (q->inp - q->outp) % q->size;
}
```

In `put` when we do `q->inp = (q->inp + 1) % q->size` we get `q->inp = (0 + 1) %
1 == 0` and then when we calculate the `size` we get `(0 - 0) % 1 == 0`. One way
to fix this is to make `q->size` be `n + 1` rather than `n` where `n` is the
size parameter of `new`, that way `put` will do `q->inp = (0 + 1) % 2 == 1`
instead and size will be `1 - 0 % 2 == 1` which is correct. Here's the diff:

```diff
  Queue *new(int n) {
-   int *buff = malloc(n*sizeof(int));
-   Queue q = {buff,0,0,n};
+   int *buff = malloc((n + 1)*sizeof(int));
+   Queue q = {buff,0,0,n + 1};
    Queue *qptr = malloc(sizeof(Queue));
    *qptr = q;
    return qptr;
}
```

As before, we can add a regression test for the size issue as follows:

```haskell
unit_queueSize :: IO ()
unit_queueSize = quickCheck (withMaxSuccess 1 (prop_queue cmds))
  where
    cmds = Commands
      [ New 1
      , Put (Var 0) 0
      , Size (Var 0)
      ]
```

After the change to `new` this test passes, but if we rerun the property we get
the following error:

```
*** Failed! Assertion failed (after 38 tests and 12 shrinks):
Commands {unCommands = [New 1,Put (Var 0) 0,Get (Var 0),Put (Var 0) 0,Size (Var 0)]}
New 1 --> New_ (Queue 0x00007fd47c00a920)
Put (Var 0) 0 --> Put_ ()
Get (Var 0) --> Get_ 0
Put (Var 0) 0 --> Put_ ()
Size (Var 0) --> Size_ (-1)
Expected: Size_ 1
Got: Size_ (-1)
```

After the second `put` we'll have `q->inp = (1 + 1) % 2 == 0` while `q->outp =
1` due to the `get` and so when we call `size` we get `0 - 1 % 2 == -1`. Taking
the absolute value:

```diff
  int size(Queue *q) {
-   return (q->inp - q->outp) % q->size;
+   return abs(q->inp - q->outp) % q->size;
  }
```

Makes this test case pass, and in fact it also makes the property pass:

```
>>> quickCheck prop_queue
+++ OK, passed 100 tests:
93% New
79% Put
74% Size
59% Get

Commands (2340 in total):
32.09% New
29.06% Size
28.25% Put
10.60% Get
```

John says that at this point most programmers would probably be happy and
believe that their implementation works, but if we rerun it again (or increase
the amount of tests generated), we get:

```
>>> quickCheck prop_queue
*** Failed! Assertion failed (after 56 tests and 19 shrinks):
Commands {unCommands = [New 2,Put (Var 0) 0,Put (Var 0) 0,Get (Var 0),Put (Var 0) 0,Size (Var 0)]}
New 2 --> New_ (Queue 0x00007fbf4c006490)
Put (Var 0) 0 --> Put_ ()
Put (Var 0) 0 --> Put_ ()
Get (Var 0) --> Get_ 0
Put (Var 0) 0 --> Put_ ()
Size (Var 0) --> Size_ 1
Expected: Size_ 2
Got: Size_ 1
```

We can see that all queues of size `1` now work, because this test starts by
creating a queue of size `2`, so we've made progress. But taking the absolute
value isn't the correct way to calculate the size (even though it works for
queues of size `1`), the following is the correct way to do it:

```diff
  int size(Queue *q) {
-   return abs(q->inp - q->outp) % q->size;
+   return (q->inp - q->outp + q->size) % q->size;
  }
```

With this final tweak, the property passes. I hope that this somewhat long
example gives you a feel for how property-based testing drives the development
and debugging of the code.

#### Example: jug puzzle from Die Hard 3

In the movie Die Hard 3 there's a
[scene](https://www.youtube.com/watch?v=BVtQNK_ZUJg) where Bruce Willis and
Samuel L. Jackson have to solve a puzzle in order to stop a bomb from going off.
The puzzle is: given a 3L and a 5L jug, how can you measure exactly 4L?

I first saw this example solved using TLA+ and I wanted to include it here
because it shows that we don't necessarily need a real implementation, merely
running the model/fake can be useful.

The main idea is to model the two jugs and all actions we can do with them and
then throw an exception when the big jug contains 4L. This will fail the test
and output the shrunk sequence of actions that resulted in the failure, giving
us the solution to the puzzle.

```{.haskell include=src/Example/DieHard.hs snippet=DieHard}
```

When we run `quickcheck prop_dieHard` we get the following output:

```
   +++ OK, failed as expected. Assertion failed (after 199 tests and 11 shrinks):
    Commands [FillBig,BigIntoSmall,EmptySmall,BigIntoSmall,FillBig,BigIntoSmall]
    FillBig --> Done

        State: Model {bigJug = 5, smallJug = 0}

    BigIntoSmall --> Done

        State: Model {bigJug = 2, smallJug = 3}

    EmptySmall --> Done

        State: Model {bigJug = 2, smallJug = 0}

    BigIntoSmall --> Done

        State: Model {bigJug = 0, smallJug = 2}

    FillBig --> Done

        State: Model {bigJug = 5, smallJug = 2}

    BigIntoSmall --> Done

        State: Model {bigJug = 4, smallJug = 3}

    Expected: BigJugIs4
    Got: Done
```

Notice how the trace shows the intermediate states, making it easy to verify
that it's indeed a correct solution to the puzzle[^3].

### Parallel property-based testing

Let's now turn our focus to parallel property-based testing.

Debugging buggy concurrent code is not fun. The main reason for this is that the
threads interleave in different ways between executions, making it hard to
reproduce the bug and hard to verify that a bug fix actually worked.

Ideally we'd like to make working with concurrent code as pleasant as the
sequential stateful case and without the user having to write any additional
test code. In order to explain how we can achieve this, we need to first
understand how we can test concurrent code in a reproducible way.

Recall our `Counter` that we looked at in the sequential testing case. Here
we'll be using a slight generalisation where the `incr` takes an integer
parameter which specifies by how much we want to increment (instead of always
incrementing by `1`).

When we interact with the counter sequentially, i.e. one command at the time,
then it appears to count correctly:

```haskell
>>> incr 1
>>> incr 2
>>> get
3
```

But if we instead concurrently issue the `incr`ements , we see something
strange:

```haskell
 > forM_ [0..100000] $ \i -> do
 >   c <- newCounter
 >   concurrently_ (incr c 1) (incr c 2)
 >   x <- get c
 >   if x == 3 then return () else error ("i = " ++ show i ++ ", x = " ++ show x)
 *** Exception: i = 29768, x = 1
```

After 29768 iterations we get back `1` rather than the expected `3`! The reason
for this is because there's a race condition in the implementation of `incr`:

```haskell
 incr i = do
   j <- readIORef gLOBAL_COUNTER
   writeIORef gLOBAL_COUNTER (i + j)
```

Because we first read the old value and _then_ write the new incremented value
in an non-atomic way, it's possible that if two threads do this at the same time
they overwrite each others increment. For example, consider the interleaving:

```
   thread 1, incr 1     |  thread 2, incr 2
   ---------------------+------------------
    0 <- readIORef      |
                        | 0 <- readIORef
                        | writeIORef (2 + 0)
    writeIORef (1 + 0)  |
                        |
                        v
                       time
```

If we read from the counter after the two increments are done we get `1` instead
of the expected `3`. The fix to this problem is to do an atomic update using
`atomicModifyIORef'`, instead of first reading and then writing to the `IORef`.

The concurrent test that we just wrote is not only specific to the counter
example but also only uses three fixed commands, the two concurrent `incr`ements
followed by a `get`. While it was enough to find this race condition, in general
we'd like to try arbitrary combinations of commands and possibly involving more
than two threads.

The key concept we need in order to accomplish that is that of *concurrent
history*, which is perhaps easiest to explain in terms of a more familiar
concept: a sequence diagram.

Consider the following sequence diagram:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/sequence-diagram.svg"
width=60%>

Here we see that the first and second thread concurrently increment, the first
thread then reads the counter concurrently with the second thread's increment
that's still going on. The second thread's increment finishes and a third thread
does a read which is concurrent with the first thread's read.

We can abstract away the arrows and merely focus on the intervals of the
commands:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/history-from-sequence-diagram.svg"
width=60%>

If we rotate the intervals we get the concurrent history:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/concurrent_counter.svg"
width=60%>

Note that the execution of some commands overlap in time, this is what's meant
by concurrent and arguably it's easier to see the overlap here than in the
original sequence diagram.

We've also abstracted away the counter, it's a black box from the perspective of
the threads. The only thing we know for sure is when we invoked the operation
and when it returned, which is what our interval captures. We also know that the
effect of the operation must have happened sometime within that interval.

One such concurrent history can have different interleavings, depending on when
exactly the effect of the commands happen. Here are two possible interleavings,
where the red cross symbolises when the effect happened (i.e. when exactly the
counter update its state).

The first corresponds to the sequential history `< incr 1, get, incr 2, get >`:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/concurrent_counter_get_1_3.svg"
width=60%>

and the other interleaving corresponds to the sequential history `< incr 1, incr
2, get, get >`:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/concurrent_counter_get_3_3.svg"
width=60%>

One last thing we've left out from the concurrent history so far is the
responses. In this example, the only interesting responses are those of the
`get`s.

Let's say that the `get`s returned `1` and `3` respectively. Is this a correct
concurrent outcome? Yes, according to linearisability it's enough to find a
single interleaving for which the sequential state machine model can explain the
outcome and in this case the first interleaving above `< incr 1, get, incr 2,
get >` does that.

What if the `get`s both returned `3`? That's also correct and witnessed by the
second interleaving `< incr 1, incr 2, get, get >`. When we can find a
sequential interleaving that supports the outcome of a concurrent execution we
say that the concurrent history linearises.

If the `get` on the third thread returned `1` or `2` however, then it would be a
non-linearisable outcome. We can see visually that that `get` happens after both
`incr`, so no matter where we choose to place the red crosses on the `incr`s the
effects will happen before that `get` so it must return `3`. Is it even possible
that `1` or `2` are returned? It's, imagine if `incr` is implemented by first
reading the current value then storing the incremented value, in that case there
can be a race where the `incr`s overwrite each other.

So to summarise, we execute commands concurrently using several threads and
gather a concurrent history of the execution. We then try to find a sequential
interleaving (a choice of where the red crosses in the diagrams should be) which
respects the a sequential state machine model specification. If we find a single
one that does, then we say that the history linearises and that the concurrent
execution is correct, if we cannot find a sequential interleaving that respects
the model then the history doesn't linearise and we have found a problem.

#### Parallel library implementation

Let's try to implement the above. We'll split up the implementation in three
parts. First, we'll show how to generate and shrink parallel commands, these
will be different than the sequential commands as we have more than one thread
that does the execution. Second, we'll have a look at how to execute the
generated parallel commands to produce a concurrent history. Finally, we'll
implement linearisability checking by trying to find an interleaving of the
concurrent history which respects the sequential model.

##### Parallel program generation and shrinking

First we need define what a parallel program is:

```{.haskell include=src/Parallel.hs snippet=ParallelCommands}
```

The idea is that the commands inside `Fork`s get executed in parallel, this list
will only be between one and three commands long, i.e. capturing single, double
or triple threaded execution. The amount of `Fork`s themselves vary with the
size of the test case, just like when we were doing the sequential testing.

Depending on the order in which the commands in the `Fork`s get executed, we can
potentially get different models. For example `Fork [Write "a" "foo", Write
"a" "bar"]`, depending on which branch of the `Fork` gets executed first we
might end up with either `"foo"` or `"bar"` being written to `"a"`.

Because of this, we have generalised generation and shrinking to work on a set
of states rather than just a single state:

```{.haskell include=src/Parallel.hs snippet=ParallelModel}
```

Notice that the default implementation for generation, which should be good
enough for most examples, picks an arbitrary state and reuses the generation
function from the sequential case. Similar shrinking picks the biggest state
(determined by the `Ord` instance) as the default implementation. The user is
able to override these defaults, in case generation or shrinking depends on some
more specific state.

We can now write a generator for parallel programs.

```{.haskell include=src/Parallel.hs snippet=arbitrary}
```

Where `nextStates` gives all potential next states and is defined as follows.

```{.haskell include=src/Parallel.hs snippet=nextStates}
```

The other helper function that we need for generation is `parallelSafe`, which
requires a bit of background.

In the sequential case a precondition is a contract that needs to be fulfilled
by the client before the command is issued. In the parallel case there are
multiple clients, so it could be the case that one client unknowingly breaks
another clients precondition. For example `Fork [Write "a" "foo", Delete "a"]`,
where the precondition for both commands is that `"a"` exists. If `Delete` gets
executed first then it would break `Write`'s precondition.

The solution to the precondition problem is to check that they hold in all
possible interleavings of a `Fork`, which is what `parallelSafe` does:

```{.haskell include=src/Parallel.hs snippet=parallelSafe}
```

While shrinking we also use `parallelSafe`:

```{.haskell include=src/Parallel.hs snippet=shrink}
```

In addition we also check that shrinking doesn't create any scoping issues, i.e.
if we remove a command which creates a symbolic variable we also need to remove
any fork that contains a command which uses said symbolic variable.

Another option is to skip the scope checking and instead require the user to
explicitly require preconditions which ensure the scope[^4].

##### Parallel running

One final difference between the sequential and the parallel case is that
because of the use of threads to achieve parallel execution, and the fact we can
only spawn threads of things of type `IO`, we also need to be able to interpret
our `CommandMonad` into `IO`, which is what `runCommandMonad` (which is also
part of the `ParallelModel` type class) does.

```{.haskell include=src/Parallel.hs snippet=runCommandMonad}
```

We can now implement parallel execution of commands as follows:

```{.haskell include=src/Parallel.hs snippet=History}
```

```{.haskell include=src/Parallel.hs snippet=runParallelCommands}
```

Extending the environment in the parallel case requires an atomic counter in
order to avoid more than one thread adding the same variable:

```{.haskell include=src/Parallel.hs snippet=env}
```

Hopefully the execution part is clear, next let's have a look at how we check
the result of an execution.

##### Linearisability checking

Recall from our parallel counter example in the introduction to parallel testing
that it's enough to find *any* possible interleaving which respects the
sequential model. So let's start by enumerating all possible interleavings using
a [`Rose`
tree](https://hackage.haskell.org/package/containers-0.7/docs/Data-Tree.html)
data structure:

```{.haskell include=src/Parallel.hs snippet=interleavings}
```

We can then check if there is a path through this rose tree which agrees with
the sequential model:

```{.haskell include=src/Parallel.hs snippet=linearisable}
```

#### Example: parallel counter

Having defined the `ParallelModel` interface (which depends on the `StateModel`
interface from the sequential testing) and programmed our parallel generation,
shrinking and parallel execution and linearisability checking against this
interface, we basically get parallel testing for free by reusing the sequential
model.

##### Testing

This is the only new code we need to add to enable parallel testing of our
`Counter` example[^5] from before:

```{.haskell include=src/Example/Counter.hs snippet=parallel-counter}
```

If we run the above property with `runReal`

```{.haskell include=src/Example/Counter.hs snippet=runReal}
```

being implemented using an increment with a race condition:

```haskell
incrRaceCondition :: IO ()
incrRaceCondition = do
  n <- readIORef gLOBAL_COUNTER
  writeIORef gLOBAL_COUNTER (n + 1)
```

then a failure is found:

```
 Assertion failed (after 36 tests and 1 shrink):
      ParallelCommands [Fork [Incr,Incr],Fork [Incr],Fork [Get,Get,Get],
                        Fork [Incr],Fork [Get,Get],Fork [Get,Get],
                        Fork [Incr],Fork [Get,Get,Incr],Fork [Incr],
                        Fork [Get,Get,Get],Fork [Incr,Incr],
                        Fork [Incr,Get],Fork [Incr,Incr],Fork [Get],
                        Fork [Incr]]
```

But shrinking didn't work very well. The reason for this is that QuickCheck
tries a smaller test case (which still has the race condition), but because of a
different interleaving of threads the race doesn't get triggered and so
QuickCheck thinks it found the minimal test case (because the smaller test case,
that the shrinker picked, passes).

The proper solution to this problem is to use a deterministic thread scheduler,
this is what they do the parallel testing
[paper](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf).
A simpler workaround is to introduce a small sleep after each read or write to
shared memory, this will make it more likely that the same interleaving happens
when we shrink the test:

```{.haskell include=src/Example/Counter.hs snippet=incrRaceCondition}
```

With this change we get the minimal test case that triggers the race condition:

```
Assertion failed (after 6 tests and 4 shrinks):
      ParallelCommands [Fork [Incr,Incr],Fork [Get]]
```

We can avoid having to sprinkle sleeps around our interaction with shared state
by creating a module with the same operations as on shared memory where the
sleep is already included:

```{.haskell include=src/SleepyIORef.hs snippet=SleepyIORef}
```

That way if we find a race, we can change the import from `import Data.IORef` to
`import SleepyIORef` and rerun the tests and get better shrinking. This
situation is not ideal, but save us the trouble of having to re-implement a
scheduler. It's worth stressing that the race is found in the unmodified code
and the introduction of sleep is only needed to make the counterexample smaller.

#### Example: process registry

For a slightly more complicated example containing race conditions, let's have a
look at an implementation of the Erlang process registry[^6].

##### Software under test

The idea behind Erlang's process registry is that you can spawn threads,
register the `ThreadId` to some name of type string, and then lookup the thread
by name rather than its thread id. Threads can also be unregistered and killed.

This is useful if threads die and get restarted and register the same name, then
other threads can easily find the thread id of the new thread using the
registry.

```{.haskell include=src/Example/Registry/Real.hs snippet=RegistryRealRace}
```

##### Model

The model of the process registry contains three things that we haven't seen
before. The first thing to note is that `Register` and `Unregister` might fail,
so we use an `Either ErrorCall` in their respective responses and a function
called `abstractError` to make the error from the model and the error from the
real implementation match up. It's called abstract, because it abstracts away
details from the real implementation, and it's a useful technique to know as it
can be applied to other settings.

The second thing to notice is the use of `monitoring` to keep statistics of how
often `Register` and `Unregister` actually do fail, this is useful coverage
information that ensures that our generators and or pre-conditions are not too
restrictive.

```{.haskell include=src/Example/Registry/Test.hs snippet=Registry}
```

The last new thing to note here is that `WhereIs_` returns the thread id that we
wanted to look up, but thread ids also happen to be references. The way we
implemented extending the environment with new references is that we call
`Data.Foldable.toList` on all responses, which gives us all references from the
responses. In the `Spawn_` case this does the right thing, since spawn returns a
reference to the newly spawned thread id, but in this case the thread id from
`WhereIs_` is not a new reference (it's merely a reference to the thread id we
wanted to look up), so we shouldn't extend the environment with the reference
that `WhereIs_` returns. We solve this problem with wrapping the response of
`WhereIs_` in `NonFoldable` which has a `toList` which doesn't return anything.

```{.haskell include=src/Stateful.hs snippet=NonFoldable}
```

##### Testing

We can now write our sequential testing property as we've done earlier for the
other examples.

```{.haskell include=src/Example/Registry/Test.hs snippet=prop_registry}
```

This property passes and we can see, thanks to our `monitoring`, that we got
good coverage of failing commands as well:

      +++ OK, passed 100 tests:
      83% Spawn
      82% WhereIs
      79% Unregister
      78% UnregisterFailed
      70% Kill
      70% Register
      62% RegisterFailed
      59% RegisterSucceeded
      29% UnregisterSucceeded

To make sure everything works as expected, let's introduce a bug on purpose:

```diff
  register :: String -> ThreadId -> IO ()
  register name tid = do
    ok <- alive tid
    reg <- readRegistry
    if ok && name `notElem` map fst reg && tid `notElem` map snd reg
      then atomicModifyIORef' registry $ \reg' ->
             if name `notElem` map fst reg' && tid `notElem` map snd reg'
-              then ((name,tid):reg',())
+              then ([(name,tid)],())
               else (reg',badarg)
      else badarg

```

If we rerun the tests with this bug in place, we get test failures like the
following:

```
     *** Failed! Assertion failed (after 30 tests and 7 shrinks):
      Commands {unCommands = [Spawn,Spawn,Register "e" (Var 1),Register "d" (Var 0),Unregister "e"]}
      Spawn --> Spawn_ (ThreadId 154)

          State: RegState {tids = [Var 0], regs = [], killed = []}

      Spawn --> Spawn_ (ThreadId 155)

          State: RegState {tids = [Var 0,Var 1], regs = [], killed = []}

      Register "e" (Var 1) --> Register_ (Right ())

          State: RegState {tids = [Var 0,Var 1], regs = [("e",Var 1)], killed = []}

      Register "d" (Var 0) --> Register_ (Right ())

          State: RegState {tids = [Var 0,Var 1], regs = [("d",Var 0),("e",Var 1)], killed = []}

      Unregister "e" --> Unregister_ (Left bad argument)

          State: RegState {tids = [Var 0,Var 1], regs = [("d",Var 0)], killed = []}

      Expected: Unregister_ (Right ())
      Got: Unregister_ (Left bad argument)
```

As we can see, unregister fails when it in fact should succeed. We've registered
`"e"` so we should be allowed to unregister it, but the real implementation has,
due to the bug, forgot that the registration happened.

Let's move on to the parallel tests, all we need to add is:


```{.haskell include=src/Example/Registry/Test.hs snippet=ParallelRegistry}
```

When we run the tests we get rather long counterexamples:

```
      *** Failed! (after 24 tests and 7 shrinks):
      Exception:
        bad argument
        CallStack (from HasCallStack):
          error, called at src/Example/Registry/Real.hs:69:10 in stateful-pbt-with-fakes-0.0.0-inplace:Example.Registry.Real
      ParallelCommands [Fork [Spawn,WhereIs "a"],Fork [Spawn],
                        Fork [Register "c" (Var 1),Spawn],Fork [Register "e" (Var 2),Register "a" (Var 2)]]
```

But if we replace our shared memory operations with version that do a bit of
sleep beforehand:

```diff
- import Data.IORef
+ import SleepyIORef
```

We get better shrinking results:

```
      *** Failed! (after 5 tests and 5 shrinks):
      Exception:
        bad argument
        CallStack (from HasCallStack):
          error, called at src/Example/Registry/Real.hs:69:10 in stateful-pbt-with-fakes-0.0.0-inplace:Example.Registry.Real
      ParallelCommands [Fork [Spawn],Fork [Register "b" (Var 0),Register "c" (Var 0)]]
```

Here we see clearly that there's some problem in `Register`, as that's the only
thing that happens in parallel. If we look at the implementation of `register`
it's obvious where the race condition is, for example we are using
`atomicModifyIORef` to update the registry. The problem is that we call
`readRegistry` to check if a name has already been registered and then call
`atomicModifyIORef`, so the race can be if another thread sneaks in between
those two calls.

We can fix this problem by adding a global lock around `register`:

```{.haskell include=src/Example/Registry/Real.hs snippet=registerNoRace}
```

When rerunning the tests with this fixed version of `registry`, we get:

```
      *** Failed! Assertion failed (after 30 tests and 13 shrinks):
      ParallelCommands [Fork [Spawn],Fork [Spawn],Fork [Spawn],
                        Fork [Register "d" (Var 2)],Fork [Unregister "d",Unregister "d"]]
```

Which seems to suggest that we have a similar problem with `unregister`, which
is indeed the case. After applying the same fix to `unregister`, we get:

```
      *** Failed! Assertion failed (after 15 tests and 4 shrinks):
      ParallelCommands [Fork [Spawn],Fork [Register "d" (Var 0)],
                        Fork [Kill (Var 0),Register "e" (Var 0)]]
```

Killing a thread will unregister it, so we get a similar problem again. If we
take the lock before calling `kill`, then the parallel tests finally pass.

These race conditions are essentially variants on the parallel counter bug, but
I hope you agree that they're not as obvious in the process registry case. I
also hope that by now it's clear that as a user we get these parallel tests
basically without doing any extra work. All the heavy lifting is done by the
library by reusing the sequential model, and this code can be written once and
then reused for all our parallel testing examples!

### Integration testing with contract tested fakes

Throughout this post we've used in-memory models, or fakes, as reference
implementations to test against.

The use of fakes diverges from the original work on Erlang QuickCheck, where a
more traditional state machine specification is used with post-conditions.

As far as I know, Edsko de Vries'
[post](https://www.well-typed.com/blog/2019/01/qsm-in-depth/) (2019) was the
first to propose the use of fakes instead of state machine specifications with
post-conditions. Edsko also showed how one can implement fake-based
specifications on top of a library that uses state machine specifications.

XXX: Post-conditions are more general than fakes? Relational vs functional?

Fake instead of state machine spec is not only easier for programmers
unfamiliar with formal specification

But there are other advantages to having a fake, for example we can use this
fake in integration tests with components that depend on the software that we
tested with the fake.

One of the problems with integration testing against fakes is that the fake can
be wrong. The standard solution to solve that problem is to [contract
test](https://martinfowler.com/bliki/ContractTest.html) the fake to make sure
that it is faithful to the software it's supposed to be a fake of. We don't have
this problem, because our tests assure that the fake is faithful.

This, final, section is about unpacking and giving examples of how integration
testing against fakes works.

#### Example: queue (again)

As our first example of integration testing, let's recall our queue example from
the section on stateful testing. We can introduce an interface for it as
follows:

```{.haskell include=src/Example/Queue/Interface.hs snippet=IQueue}
```

The real implementation can instantiate this interface in a straightforward way:

```{.haskell include=src/Example/Queue/Interface.hs snippet=real}
```

The interesting part is that our fake can also instantiate the same interface by
storing the state in a mutable reference (`IORef`) as follows.

```{.haskell include=src/Example/Queue/Interface.hs snippet=fake}
```

We can now write components or services *against* this interface:

```{.haskell include=src/Example/Queue/Interface.hs snippet=prog}
```

When we integration test our new component we can use the `fake` instance to
make the tests fast and deterministic, while when we deploy we use the `real`
instance and because of our stateful property-based tests we know that the fake
is faithful to the real implementation.

#### Example: file system

The next example is a file system, first used by Edsko de Vries in the
[post](https://www.well-typed.com/blog/2019/01/qsm-in-depth/) (2019) that also
introduced using fakes as models.

The interface is parametrised by a file handle. We can create directories, open
files to get a hold of a file handle, file handles can then be read from and
written to, and finally closed:

```{.haskell include=src/Example/FileSystem/Interface.hs snippet=IFileSystem}
```

The real implementation of this interface uses the real file system. In order to
isolate the tests all operations will be relative to some `root` directory:


```{.haskell include=src/Example/FileSystem/Real.hs snippet=FileSystemReal}
```

```{.haskell include=src/Example/FileSystem/Interface.hs snippet=real}
```

The fake implementation of the interface is, as usual, implemented using an
in-memory data structure:

```{.haskell include=src/Example/FileSystem/Fake.hs snippet=FileSystemFake}
```

```{.haskell include=src/Example/FileSystem/Interface.hs snippet=fake}
```

Assuming we've tested that the fake file system is faithful to the real one, we
can depend on the interface in all components of our system that need the file
system:

```{.haskell include=src/Example/FileSystem/Interface.hs snippet=prog}
```

We can then use the fake file system when we integration test and thus get fast
and deterministic tests, and then use the real file system when we deploy.

```{.haskell include=src/Example/FileSystem/Interface.hs snippet=testDeploy}
```

Because of the fact that we know that the fake is faithful to the real file
system implementation, we can be relatively sure that swapping in the real file
system instead of the fake one when deploying will not introduce bugs. If it
does introduce a bug then we have a mismatch between the fake and the real
implementation and we need to investigate how it slipped through our stateful
property-based
[tests](https://github.com/stevana/stateful-pbt-with-fakes/blob/main/src/Example/FileSystem/Test.hs).

Note that `prog` is just a silly example, in a real system the component that
uses the file system can be more complex, for example in the system that Edsko
worked on the component that depended on the file system is a database. In such
cases it makes sense to write a whole new stateful and parallel property-based
test suite using database commands and responses, it's those tests that do the
integration testing between the database and the fake file system, while the
stateful and parallel property-based tests of the file system alone do the
contract tests that ensure that the file system fake is faithful to the real
file system.

#### Example: bigger system of components

The examples given above, a queue and a file system, might not seems necessary
to fake[^7] so to finish of let's sketch how the same technique scales to a
bigger system of components or services.

Imagine we have three components or services, where component *A* depends on
component *B* which depends on component *C*:

```
  +---+      +---+      +---+
  |   |      |   |      |   |
  | A +----->| B +----->| C |
  |   |      |   |      |   |
  +---+      +---+      +---+

```

Following the pattern that we did for the queue and file system example, we'd
define three interfaces:


```haskell
data IA = ...
data IB = ...
data IC = ...
```

And the dependencies are made clear when we instantiate the interfaces:

```haskell
iC :: IO IC       -- C has no dependencies.
iB :: IC -> IO IB -- B depends on C.
iA :: IB -> IO IA -- A depends on B.
```

The testing strategy is then as follows:

1. Stateful and parallel test C, this gives us a fake of C which is contract
   tested;
2. Use C fake when integration testing B;
3. Use B fake (which uses the C fake) when testing A.

Hopefully it should be clear that this strategy scales to more components or
services[^8].

## Conclusion and future work

We've seen how stateful and parallel property-based testing can be implemented
in about 400 lines of code, which is comparable to the 300 lines of code of the
first version of QuickCheck (which didn't have shrinking). We've also had a look
at several examples of how we can use fakes as models and how to test bigger
systems in a compositional manner by reusing the fakes.

I hope that this is enough material to get people curious and experimenting in
other programming languages. I used Haskell, because it's what the original
QuickCheck library is written in, but I think it would be good to translate code
to other programming language paradigms, thus making it easier for others to
learn and experiment. If anyone is interested in starting such a port to a
different language, then I'd be happy to help. Feel free to open issues and ask
questions in the the [code
repository](https://github.com/stevana/stateful-pbt-with-fakes) of this post.

I've also writen down a bunch of
[ideas](https://github.com/stevana/stateful-pbt-with-fakes/blob/main/TODO.md)
for improvements and further exploration, in case anyone's interested in digging
deeper into this topic (again, happy to elaborate, feel free to get [in
touch](https://stevana.github.io/about.html)).

## Acknowledgments

I'd like to thank Daniel Gustafsson for helping implement the
`quickcheck-state-machine` library with me seven years ago, discussing a fix for
parallel commands generation
[issue](https://github.com/stevana/quickcheck-state-machine/issues/51) that I
found while writing this post, and for proofreading.


[^1]: Is there a source for this story? I can't remember where I've heard it.
    This short
    [biography](http://www.erlang-factory.com/conference/London2011/speakers/JohnHughes)
    gives some of the details:

    > "From 2002-2005 he led a major research project in software verification,
    > funded by the Swedish Strategic Research Foundation. This led to the
    > development of Quviq QuickCheck in Erlang."

    I believe [this](https://strategiska.se/forskning/genomford-forskning/ramanslag-inom-it-omradet/projekt/2010/)
    must be the project mentioned above.

[^2]: There's some room for error here from the users side, e.g. the user could
    create non-unique references. In a proper library one might want to
    introduce a `genSym` construct which guarantees uniqueness.

[^3]: So stateful property-based testing with a trivial `runReal` can be seen as
    crude version of a random path exploring "model checker". One could perhaps
    implement something closer to TLC (the model checker for TLA+), which
    enumerates all paths up to some depth, by using `smallcheck` rather than
    `QuickCheck`. If this topic interests you, you might also want to have a
    look at Gabriella Gonzalez's
    [HasCal](https://github.com/Gabriella439/HasCal).

    I don't have an example for this, but I guess one can also think of stateful
    property-based testing with a trivial `runFake` as a crude version of a
    fuzzer (without coverage guidance). For more on this and how to add coverage
    guidance, see [*Coverage guided, property based
    testing*](https://dl.acm.org/doi/10.1145/3360607) (2019).

[^4]: Another idea might be to drop all preconditions in the parallel case and
    make all commands be able to fail gracefully instead of crashing, e.g.
    `Write_ (Either DoesntExist ())`.

    The problem with this approach is that some examples, such as the [ticket
    dispenser](https://github.com/stevana/stateful-pbt-with-fakes/blob/main/src/Example/TicketDispenser.hs),
    have initialisation commands such as `New` which create a ticket dispenser
    reference upon which the later commands depend on, so without preconditions
    forbidding more than one `New` we can end up generating: `Fork New New`,
    which doesn't make sense. It should also be noted that making `New` fail
    gracefully when a `New` has already been executed would need a global
    boolean flag, which is ugly.

[^5]: The parallel counter example is very similar to the [ticket
    dispenser](https://github.com/stevana/stateful-pbt-with-fakes/blob/main/src/Example/TicketDispenser.hs)
    example that appears in John's paper [*Testing the hard stuff and staying
    sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
    (2014).

[^6]: The sequential variant of the process registry example first appeared in
    the paper [*QuickCheck testing for fun and
    profit*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
    (2007) and is also part of John's Midlands Graduate School course (2019).
    The parallel tests were introduced in [*Finding Race Conditions in Erlang
    with QuickCheck and
    PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
    (2009).

[^7]: Unless we want to test what happens when failures, such as the disk being
    full etc.
    [Research](http://www.eecg.toronto.edu/~yuan/papers/failure_analysis_osdi14.pdf)
    shows that "almost all (92%) of the catastrophic system failures are the
    result of incorrect handling of non-fatal errors explicitly signaled in
    software. [...] in 58% of the catastrophic failures, the underlying faults
    could easily have been detected through simple testing of error handling
    code.". Fakes make it easier to inject faults, but that's a story for
    another day.

[^8]: See the talk [Integrated Tests Are A
    Scam](https://www.youtube.com/watch?v=fhFa4tkFUFw) by J.B. Rainsberger for a
    longer presentation of this idea.
