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
what the former group's main interest is. The latter group's mostly conserned
with a branch of functional programming where the type system is sufficiently
expressive that it allows for formal specifications of programs, sometimes
called dependently typed programming or type theory. Agda is an example of a
Haskell-like dependently typed programming language, that also happens to be
mainly developed by the Programing Logic group. Given the overlap of interest
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
is likely to be correct). As programs get more complicted the ratio of effort
saved by merely testing, as opposed to proving, increases. In fact for bigger
programs the effort involved in proving correctness is simply too high for it to
be practical (this is an active area of research). Given all this, I hope that
you can start to see why this idea excited John.

While John was working on this idea, Koen Claessen, another member of the
Functional Programing group, [stuck his
head](https://youtu.be/x4BNj7mVTkw?t=289) into John's office and asked what he
was doing. Koen got excited as well and came back the next day with his improved
version of John's code. There was some things that Koen hadn't thought about, so
John iterated on his code and so it went back and forth for a week until the
first implementation of property-based testing was written and not long
afterwards they publised the paper [*QuickCheck: A Lightweight Tool for Random
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

While the 2007 paper also mentiones *parallel* testing via traces and
interleavings, it's vague on details. It's only later in [*Finding Race
Conditions in Erlang with QuickCheck and
PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
(ICFP 2009) that parallel testing is described in detail including a reference
to [*Linearizability: a correctness condition for concurrent
objects*](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf) (1990) which is
the main technique behind it.

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

Sometimes various minor extenions to stateful and parallel testings are needed
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

To my best knowledge, as of April 2024, the following table summarises the
situation. Please open an
[issue](https://github.com/stevana/stateful-pbt-with-fakes/issues), PR, or get
in [touch](https://stevana.github.io/about.html) if you see a mistake or an
important omission.

| Library | Language | Stateful | Parallel | Notes |
| :---    | :---     | :---:    | :---:    | :---  |
| Eris | PHP | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | |
| FsCheck | F# | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | Has experimental [stateful testing](https://fscheck.github.io/FsCheck//StatefulTestingNew.html). An [issue](https://github.com/fscheck/FsCheck/issues/214) to add parallel support has been open since 2016. |
| Gopter | Go | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | The README says "No parallel commands ... yet?" and there's an open [issue](https://github.com/leanovate/gopter/issues/20) from 2017. |
| Hedgehog | Haskell | <ul><li>- [x] </li></ul> | <ul><li>- [x] </li></ul> | Has parallel support, but the implementation has [issues](https://github.com/hedgehogqa/haskell-hedgehog/issues/104). |
| Hypothesis | Python | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | |
| PropEr | Erlang | <ul><li>- [x] </li></ul> | <ul><li>- [x] </li></ul> | First open source library to support both? |
| QuickCheck | Haskell | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | There's an open [issue](https://github.com/nick8325/quickcheck/issues/139) to add stateful testing since 2016. |
| QuickTheories | Java | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | Has [experimental](https://github.com/quicktheories/QuickTheories/issues/42) for stateful testing, there's also some parallel testing, but it's inefficient and restrictive compared to QuviQ's Erlang version of QuickCheck. From the [source code](https://github.com/quicktheories/QuickTheories/blob/a963eded0604ab9fe1950611a64807851d790c1c/core/src/main/java/org/quicktheories/core/stateful/Parallel.java#L35): "Supplied commands will first be run in sequence and compared against the model, then run concurrently. All possible valid end states of the system will be calculated, then the actual end state compared to this. As the number of possible end states increases rapidly with the number of commands, command lists should usually be constrained to 10 or less." |
| Rapid | Go | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | |
| RapidCheck | C++ | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | There's an open [issue](https://github.com/emil-e/rapidcheck/issues/47) to add parallel support from 2015. |
| ScalaCheck | Scala | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | Has some support for parallel testing, but it's limited as can be witnessed by the fact that the two [examples](https://github.com/typelevel/scalacheck/tree/19af6eb656ba759980664e29ec6ae3e063021685/examples) of testing LevelDB and Redis both are sequential (`threadCount = 1`). |
| SwiftCheck | Swift | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | There's an open [issue](https://github.com/typelift/SwiftCheck/issues/149) to add stateful testing from 2016. |
| fast-check | TypeScript | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | Has [some support](https://fast-check.dev/docs/advanced/race-conditions/) for race condition checking, but it seems different from Quviq QuickCheck's parallel testing. In particular it doesn't seem to reuse the sequential state machine model nor use linearisability. |
| jetCheck | Java | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | From the source code "Represents an action with potential side effects, for single-threaded property-based testing of stateful systems.". |
| jsverify | JavaScript | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | There's an open [issue](https://github.com/jsverify/jsverify/issues/148) to add stateful testing from 2015. |
| lua-quickcheck | Lua | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | |
| propcheck | Elixir | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | There's an open [issue](https://github.com/alfert/propcheck/issues/148) to add parallel testing from 2020. |
| proptest | Rust | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | See proptest-state-machine. |
| proptest-state-machine | Rust | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | Documentation says "Currently, only sequential strategy is supported, but a concurrent strategy is planned to be added at later point.". |
| qcheck-stm | OCaml | <ul><li>- [x] </li></ul> | <ul><li>- [x] </li></ul> | |
| quickcheck | Prolog | <ul><li>- [ ] </li></ul> | <ul><li> - [ ] </li></ul> | |
| quickcheck | Rust | <ul><li>- [ ] </li></ul> | <ul><li> - [ ] </li></ul> | Issue to add stateful testing has been [closed](https://github.com/BurntSushi/quickcheck/issues/134). |
| quickcheck-state-machine | Haskell | <ul><li>- [x] </li></ul> | <ul><li>- [x] </li></ul> | Second open source library with parallel testing support? (I was [involved](https://github.com/nick8325/quickcheck/issues/139#issuecomment-272439099) in the development.) |
| rackcheck | Racket | <ul><li>- [ ] </li></ul> | <ul><li> - [ ] </li></ul> |  |
| rantly | Ruby | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | |
| test.check | Clojure | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | Someone has implemented stateful testing in a blog [post](http://blog.guillermowinkler.com/blog/2015/04/12/verifying-state-machine-behavior-using-test-dot-check/) though. |
| theft | C | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | |

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

1. The stateful and parallel testing featurs are not as useful as testing pure
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

   > Thomas Arts and I have founded a start-up, Quviq AB, to develop and market
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
   > it seems to be succeeding.

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
(which warrents publishing a new paper) rather than general advice which can be
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
funding to work on tooling (which typically isn't reconginised as doing
research), or feedback from industry? Surely, one cannot expect research funding
agencies to pay for this?

On the other hand one could ask why there isn't a requirement that published
research should be reproducable using open source tools (or at least tools that
are freely available to the public and other researchers)?

Trying to replicate the results from the Quviq QuickCheck papers (from 2006 and
onwards) without buying a Quviq QuickCheck license, is almost impossible without
a lot of reverse engineering work.

I suppose one could argue that one could have built a business around an open
source tool, only charging for the training and consulting, but given how broken
open source is today, unless you are a big company (which takes more than it
gives back), it's definitely not clear that it would have worked (and it was
probably even worse back in 2006).

Even if John is right and that keeping it closed source has helped adoption in
industry, I think it's by now fair to say it has not helped open source
adoption.

Or perhaps rather, it's unlikely that a company that pays for a
licence in Erlang would then go and port the library in another language.

### What can we do about it?

I think there's at least two things worth trying.

1. Provide a short open source implementation of stateful and parallel
   property-based testing, analogous to the original ~300LOC vanilla QuickCheck
   implementation.

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
     about 330 lines of code (similar to the size of the original QuickCheck
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
that you might have overlooked! This is where generating random inputs, a key
feature of property-based testing, comes in. The idea being that random inputs
will eventually hit corner cases.

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
shrunk twice and the minimal counter example `[0, 1]` is presented. Notice that
we do need a list that is at least of length two, because any shorter list will
reverse to itself.

As pointed out earlier, coming up with these properties is by no means obvious.
There are however a few patterns that come up over and over again. With
`reverse` we saw an example of an involutory function, i.e. `f (f x) == x`, here
are a few other examples:

- Inverses, e.g. `\(i : Input) -> deserialise (serialise i) == i`;
- Idempotency, e.g. `\(xs : [Int]) -> sort (sort xs) == sort xs`;
- Associativity, e.g. `\(i j k : Int) -> (i + j) + k == i + (j + k)`;
- Axioms of abstract data types, e.g. `\(x : Int)(xs : [Int]) -> member x
  (insert x xs) && not (member x (remove x xs))`;
- Metamorphic properties, e.g. `\(g : Graph)(m n : Node) -> shortestPath g m n ==
  shortestPath g n m`.

Readers familiar with discrete math might recognise some of the above.

### Stateful property-based testing in ~150 LOC

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

Where `s` is the state, `i` is an input (e.g. `incr`) and `o` is an ouput.
Notice how the state evolves over time and depends on the history of inputs.

In the pure case each test case is a single input, in the stateful case we need
a sequence of inputs in order to test how the system changes over time. In the
pure case our our properties were relations on the input and output, i.e. `R : i
-> o -> Bool`. In the stateful case our properties would need to be generalised
to `R' : [i] -> [o] -> Bool` to account for how the state changes over time.
Writing such properties is cumbersome, an alternative is to account for the
state explicitly by means of some kind of model.

This model could be a state machine of type `s -> i -> (s, o)`, i.e. a function
from the old state and an input to the next state and the output. From this we
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
disk, communicating over the network, or various perform time or space
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

#### Library implementation

In the example above we implemented the `StateModel` interface (or type class),
next we'll have a look at the definiton of this interface and the testing
functionality we can derive by programming against the interface.

##### Stateful testing interface

Let me give you the full definition of the interface and then I'll explain it in
words afterwards.

```haskell
class ( ... ) => StateModel state where

  initialState :: state

  data Command  state :: Type -> Type
  data Response state :: Type -> Type

  type Reference state :: Type
  type Reference state = Void

  type PreconditionFailure state :: Type
  type PreconditionFailure state = Void

  type CommandMonad state :: Type -> Type
  type CommandMonad state = IO

  generateCommand :: state -> Gen (Command state (Var (Reference state)))

  shrinkCommand :: state -> Command state (Var (Reference state))
                -> [Command state (Var (Reference state))]
  shrinkCommand _state _cmd = []

  runFake :: Command state (Var (Reference state)) -> state
          -> Either (PreconditionFailure state)
                    (state, Response state (Var (Reference state)))

  runReal :: Command state (Reference state)
          -> CommandMonad state (Response state (Reference state))

  monitoring :: (state, state)
             -> Command state (Reference state)
             -> Response state (Reference state)
             -> Property -> Property
  monitoring _states _cmd _resp = id

  commandName :: (Show (Command state ref), Show ref)
              => Command state ref -> String
  commandName = head . words . show
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
POSIX-like filesystem, then later commands need to be able to refer to that file
handle when wanting to write or read from it. The second datatype is
`PreconditionFailure`, which is used to give a nice error message when a command
is executed in a disallowed state. For example if we try to read from a file
handle that has been closed. The third data type is `CommandMonad` which let's
us use a different monad than `IO` for executing our commands in. After we've
finished with the interface definition we'll come back to more examples where
we'll use these optional types, hopefully these examples will help make things
more concrete.

We've already seen that the user needs to provide a way to generate single
commands, the only thing worth mentioning is that in case our commands are
parametrised by references then during the generation phase we only deal with
`Var`s of references, where `data Var a = Var Int`. The reason for this is that
we cannot generate, for example, real file handles (only the operating system
can), so instead we generate symbolic references which are just `Int`s. Think of
these as placeholders for which real references will be substituted in once the
real references are created during execution.

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

```haskell
newtype Commands state = Commands
  { unCommands :: [Command state (Var (Reference state))] }
```

As mentioned above, when we generate commands we cannot generate real
references, e.g. file handles, thus `Var (Reference state)` is used which is
isomorphic to just an `Int`.

Sometimes it's convenient to split up `runFake` into two parts, the first checks
if the command is allowed in the current state, i.e. the precondition holds:

```haskell
precondition :: StateModel state
             => state -> Command state (Var (Reference state)) -> Bool
precondition s cmd = case runFake cmd s of
  Left _  -> False
  Right _ -> True
```

And the second part advances the state:

```haskell
nextState :: StateModel state
          => state -> Command state (Var (Reference state)) -> state
nextState s cmd = case runFake cmd s of
  Right (s', _) -> s'
  Left _err -> error "nextState: impossible, we checked for success in precondition"
```

We assume that we'll only ever look at the `nextState` when the `precondition`
holds.

Using these two functions we can implement QuickCheck's `Arbitrary` type class
for `Commands` which let's us generate and shrink `Commands`:

```haskell
instance StateModel state => Arbitrary (Commands state) where

  arbitrary :: Gen (Commands state)
  arbitrary = Commands <$> genCommands initialState
    where
      genCommands :: StateModel state
                  => state -> Gen [Command state (Var (Reference state))]
      genCommands s = sized $ \n ->
        let
          w = n `div` 2 + 1
        in
          frequency
            [ (1, return [])
            , (w, do mcmd <- generateCommand s `suchThatMaybe` precondition s
                     case mcmd of
                       Nothing  -> return []
                       Just cmd -> (cmd :) <$> genCommands (nextState s cmd))
            ]

  shrink :: Commands state -> [Commands state]
  shrink (Commands cmds) =
    map (Commands . prune . map fst)
        (shrinkList shrinker (snd (withStates initialState cmds)))
    where
      shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]
```

Notice how when we shrink we first compute the state and then shrink the command
at that state. We do so by help of the following helper function:

```haskell
withStates :: StateModel state
           => state -> [Command state (Var (Reference state))]
           -> (state, [(Command state (Var (Reference state)), state)])
withStates s0 = go s0 []
  where
    go s acc []           = (s, reverse acc)
    go s acc (cmd : cmds) = go (nextState s cmd) ((cmd, s) : acc) cmds
```

Also notice how after shrinking we `prune` away all commands that don't pass the
precondition:

```haskell
prune :: StateModel state
      => [Command state (Var (Reference state))] -> [Command state (Var (Reference state))]
prune = go initialState
  where
    go _s [] = []
    go  s (cmd : cmds)
      | precondition s cmd = cmd : go (nextState s cmd) cmds
      | otherwise          = go s cmds
```

The intuition here is that as we remove commands from the originally generated
`Commands` (which all pass their preconditions), we might have broken some
preconditions and pruning simply removes the commands which we made invalid in
the process of shrinking.

##### Running and assertion checking

Once we've generated `Commands` we need to execute them against the model and
the real system using `runFake` and `runReal`. In the process of doing so
`runReal` will produce `Reference`s that later commands might use, so we also
need to substitute symbolic references for real references. This, together
coverage statatistics bookkeeping, is done in the `runCommands` function:

```haskell
runCommands :: forall state. StateModel state
            => Commands state -> PropertyM (CommandMonad state) ()
runCommands (Commands cmds0) = go initialState [] cmds0
  where
    go :: state -> [(Int, Reference state)] -> [Command state (Var (Reference state))]
       -> PropertyM (CommandMonad state) ()
    go _state _env [] = return ()
    go  state  env (cmd : cmds) = do
      case runFake cmd state of
        Left _err -> do
          monitor (counterexample "Preconditon failed")
          assert False
        Right (state', resp) -> do
          let name = commandName cmd
          monitor (tabulate "Commands" [name] . classify True name)
          -- Here we substitute all symbolic references for real ones:
          let ccmd = fmap (lookupEnv env) cmd
          cresp <- run (runReal ccmd)
          monitor (counterexample (show cmd ++ " --> " ++ show cresp))
          monitor (monitoring (state, state') ccmd cresp)
          -- Here we collect all references from the response and store it in
          -- our environment, so that subsequence commands can be substituted.
          let refs   = toList cresp
              env'   = env ++ zip [length env..] refs
              cresp' = fmap (lookupEnv env') resp
              ok     = cresp == cresp'
          unless ok $
            monitor (counterexample ("Expected: " ++ show cresp' ++ "\nGot: " ++ show cresp))
          -- And finally here's where we assert that the model and the real
          -- implementation agree.
          assert ok
          go state' env' cmds

lookupEnv :: [(Int, a)] -> Var a -> a
lookupEnv env (Var x) =
  case lookup x env of
    Nothing  -> discard -- ^ This can happen if a shrink step makes a variable unbound.
    Just ref -> ref
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

```haskell
instance StateModel State where

  initialState = emptyState

  type Reference State = Queue

  type PreconditionFailure State = Err

  data Command State q
    = New Int
    | Put q Int
    | Get q
    | Size q
    deriving (Show, Functor)

  data Response State q
    = New_ q
    | Put_ ()
    | Get_ Int
    | Size_ Int
    deriving (Eq, Show, Functor, Foldable)

  generateCommand s
    | Map.null s = New . getPositive <$> arbitrary
    | otherwise  = oneof
      [ New . getPositive <$> arbitrary
      , Put  <$> arbitraryQueue <*> arbitrary
      , Get  <$> arbitraryQueue
      ]
    where
      arbitraryQueue :: Gen (Var Queue)
      arbitraryQueue = elements (Map.keys s)

  shrinkCommand _s (Put q i) = [ Put q i' | i' <- shrink i ]
  shrinkCommand _s _cmd = []

  runFake (New sz)  s = fmap New_  <$> fNew sz s
  runFake (Put q i) s = fmap Put_  <$> fPut q i s
  runFake (Get q)   s = fmap Get_  <$> fGet q s
  runFake (Size q)  s = fmap Size_ <$> fSize q s

  -- Here `new`, `put`, `get` and `size` are FFI wrappers for their respective C
  -- functions.
  runReal (New sz)  = New_  <$> new sz
  runReal (Put q i) = Put_  <$> put q i
  runReal (Get q)   = Get_  <$> get q
  runReal (Size q)  = Size_ <$> size q
```

The only new thing worth paying attention to is the `q` in `Command` and
`Response`, which is parametrised so that it works for both symbolic and real
references. The `Functor` instance let's us to substitution, while `Foldable`
let's us extract all new references from a response, so that we can substitute
them in later `Command`s.

##### Testing

```haskell
prop_queue :: Commands State -> Property
prop_queue cmds = monadicIO $ do
  runCommands cmds
  assert True
```

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

* Circular buffer rewrites the first entry, implementation is correct, model is wrong.

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

```haskell
unit_queueFull :: IO ()
unit_queueFull = quickCheck (withMaxSuccess 1 (prop_queue cmds))
  where
    cmds = Commands
      [ New 1
      , Put (Var 0) 1
      , Put (Var 0) 0
      , Get (Var 0)
      ]
```

```
*** Failed! Assertion failed (after 1 test):
New 1 --> New_ (Queue 0x00000000006f6d20)
Put (Var 0) 1 --> Put_ ()
Preconditon failed
```

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

```diff
  int size(Queue *q) {
-   return (q->inp - q->outp) % q->size;
+   return abs(q->inp - q->outp) % q->size;
  }
```

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

```diff
  int size(Queue *q) {
-   return abs(q->inp - q->outp) % q->size;
+   return (q->inp - q->outp + q->size) % q->size;
  }
```


#### Example: jug puzzle from Die Hard 3

In the movie Die Hard 3 there's an
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

```haskell
data Model = Model
  { bigJug   :: Int
  , smallJug :: Int
  }
  deriving (Eq, Show)

instance StateModel Model where

  initialState = Model 0 0

  type Reference Model = Void

  data Command Model r
    = FillBig
    | FillSmall
    | EmptyBig
    | EmptySmall
    | SmallIntoBig
    | BigIntoSmall
    deriving (Show, Enum, Bounded, Functor)

  data Response Model r = Done | BigJugIs4
    deriving (Eq, Show, Functor, Foldable)

  generateCommand :: Model -> Gen (Command Model r)
  generateCommand _s = elements [minBound ..]

  runFake :: Command Model r -> Model -> Either void (Model, Response Model r)
  runFake FillBig      s = done s { bigJug   = 5 }
  runFake FillSmall    s = done s { smallJug = 3 }
  runFake EmptyBig     s = done s { bigJug   = 0 }
  runFake EmptySmall   s = done s { smallJug = 0 }
  runFake SmallIntoBig (Model big small) =
    let big' = min 5 (big + small) in
    done (Model { bigJug = big'
                , smallJug = small - (big' - big) })
  runFake BigIntoSmall (Model big small) =
    let small' = min 3 (big + small) in
    done (Model { bigJug = big - (small' - small)
                , smallJug = small'
                })

  runReal :: Command Model Void -> IO (Response Model (Reference Model))
  runReal _cmd = return Done

  monitoring :: (Model, Model) -> Command Model Void -> Response Model Void
             -> Property -> Property
  monitoring (_s, s') _cmd _resp =
    counterexample $ "\n    State: " ++ show s' ++ "\n"

  runCommandMonad _s = id

done :: Model -> Either void (Model, Response Model ref)
done s' | bigJug s' == 4 = return (s', BigJugIs4)
        | otherwise      = return (s', Done)

prop_dieHard :: Commands Model -> Property
prop_dieHard cmds = withMaxSuccess 10000 $ monadicIO $ do
  runCommands cmds
  assert True
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
that it's indeed a correct solution to the puzzle.

### Parallel property-based testing in ~180 LOC

Let's now turn our focus to parallel property-based testing.

Before showing you the implementation, let me start off by giving a bit of
motivation, a high-level sketch of how it works, and mentioning some prior work
that I've stolen ideas from.

#### Motivation

Typically debugging buggy concurrent code is not fun. The main reason for this
is that the threads interleave in different ways between executions, making it
hard to reproduce the bug and hard to verify that a bug fix actually worked.

* Heisenbug

By generating tests, running the same tests many times and shrinking them,
parallel property-based testing tries to make it slightly less burdensome on the
programmer.

XXX:
We've seen how to test if a sequential (single-threaded) program respects some
specification.

We did so by generating a random sequence of commands and then applied them one
by one to both the real software under test (SUT), a counter, and the state
machine specification and then compared the outputs.

Counters are often shared among different threads though, for example to keep
track of some metric like current number of concurrent connections that our
service is serving.

So we might want to ask ourselves: how can we test that the counter
implementation is thread-safe?

Below we will show how the *same* state machine specification that we already
developed previously can be used to check if a concurrent execution is correct
using a technique called linearisability checking.

#### How it works

Let's first recall our counter example from last time:

```haskell
 > c <- newCounter
 > incr c 1
 > incr c 2
 > get c
 3
```

When we interact with the counter sequentially, i.e. one command at the time,
then it appears to count correctly.

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
 incr (Counter ref) i = do
   j <- readIORef ref
   writeIORef ref (i + j)
```

Because we first read the old value and _then_ write the new incremented value
in an non-atomic way, it's possilbe that if two threads do this at the same time
they overwrite each others increment. For example:

```
   thread 1, incr 1         |  thread 2, incr 2
   -------------------------+------------------
    0 <- readIORef ref      |
                            | 0 <- readIORef ref
                            | writeIORef ref (2 + 0)
    writeIORef ref (1 + 0)  |
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
by concurrent and arguebly it's easier to see the overlap here than in the
original sequence diagram.

We've also abstracted away the counter, it's a black box from the perspective of
the threads. The only thing we know for sure is when we invoked the operation
and when it returned, which is what our interval captures. We also know that the
effect of the operation must have happend sometime within that interval.

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
effects will happen before that `get` so it must return `3`. Is it even possilbe
that `1` or `2` are returned? It's, imagine if `incr` is implemented by first
reading the current value then storing the incremented value, in that case there
can be a race where the `incr`s overwrite each other.

So to summarise, we execute commands concurrently using several threads and
gather a concurrent history of the execution. We then try to find a sequential
interleaving (a choice of where the red crosses in the diagrams should be) which
respects the a sequential state machine model specfication. If we find a single
one that does, then we say that the history linearises and that the concurrent
execution is correct, if we cannot find a sequential interleaving that respects
the model then the history doesn't linearise and we have found a problem.

#### Prior work

The following resources where useful when me and my then colleague, Daniel
Gustafsson, first implemented parallel property-based testing in
[`quickcheck-state-machine`](https://github.com/stevana/quickcheck-state-machine/tree/master)
back in 2017.

Apart from the [*Finding Race Conditions in Erlang with QuickCheck and
PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
(2009) paper and the [*Linearizability: a correctness condition for concurrent
objects*](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf) (1990) paper
that they reference, it was useful to have a look at the Erlang's PropEr
library, which is a (the first?) Quviq QuickCheck clone with support for both
stateful and parallel testing.

* Not property-based testing per say, but similar in that it generates random
  commands and checks linearisability is Jepsen's
  [Knossos](https://aphyr.com/posts/309-knossos-redis-and-linearizability)

#### Implementation

XXX: extend StateModel class:

```haskell
  -- If another command monad is used we need to provide a way run it inside the
  -- IO monad. This is only needed for parallel testing, because IO is the only
  -- monad we can execute on different threads.
  runCommandMonad :: proxy state -> CommandMonad state a -> IO a
```
##### Parallel program generation and shrinking


* shrinking can be improved, see qsm

##### Linearisability checking

##### Parallel running

#### Example: parallel counter

```haskell
```

#### Example: ticket dispenser

[*Testing the hard stuff and staying
sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
(2014)

```haskell
```

#### Example: process registry

This example comes from the paper [*QuickCheck testing for fun and
profit*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
(2007) and is also part of John's Midlands Graduate School course (2019).

The parallel tests for the process registry was introduced in [*Finding Race
Conditions in Erlang with QuickCheck and
PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
(2009)

Parallel property still fails sometimes... E.g. `--quickcheck-replay=300529`

```haskell
```

### Integration testing with contract tested fakes

* Vanilla property-based testing generates unit tests, what about stateful and
  parallel property-based testing?

* Fake instead of state machine spec is not only easier for programmers
  unfamilar with formal specification, it's also more useful in that the fake
  can be used in integration tests with components that depend on the SUT

* let this be your prototype

* https://martinfowler.com/bliki/ContractTest.html
* Edsko's lockstep https://www.well-typed.com/blog/2019/01/qsm-in-depth/
* [Integrated Tests Are A Scam](https://www.youtube.com/watch?v=fhFa4tkFUFw) by J.B. Rainsberger

#### Motivation
#### How it works

#### Example: queue (again)

#### Example: file system

+ proper coverage?

#### Example: bigger system of components?

* A queue and a file system might not seem necessary to fake (unless we consider fault-injection)

```haskell

data IServiceA = ...
data IServiceB = ...
data IServiceC = ...

iServiceB :: IServiceC -> IO IServiceB
iServiceA :: IServiceB -> IO IServiceA
```

* Stateful and parallel test C, this gives us a fake of C which is contract tested
* Use C fake when integration testing B
* Use B fake (which uses the C fake) when testing A

### Prior work

#### Stateful

I'd like to explain where my inspiration is coming from, because I think it's
important to note that the code I'm about to present didn't come from thin air
(even though it might look simple).

I've been thinking about this problem since the end of 2016 as can be witnesed
by my involvement in the following
[issue](https://github.com/nick8325/quickcheck/issues/139) about adding stateful
testing to Haskell's QuickCheck.

My initial attempt eventually turned into the Haskell library
`quickcheck-state-machine`.

The version below is a combination of my experience building that library, but
also inspried by:

  1. Nick Smallbone's initial
  [version](https://github.com/nick8325/quickcheck/issues/139#issuecomment-279836475)
  (2017) from that same issue. (Nick was, and I think still is, the main
  maintainer of the original QuickCheck library);

  2. John's Midlands Graduate School
  [course](https://www.cse.chalmers.se/~rjmh/MGS2019/) (2019);

  3. Edsko de Vries' "lockstep"
  [technique](https://www.well-typed.com/blog/2019/01/qsm-in-depth/) (2019).

XXX: I'll refer back to these when I motivate my design decisions below.


## Conclusion and future work

* Stateful and parallel testing in ~330 LOC vs the 300LOC of the first version
  of QuickCheck

* How to test bigger systems in a compositional manner by reusing the fakes

* Translate code to other programming language paradigms, thus making it easier
  for library implementors

* repo where people can open issues and ask questions and explore improvements

* Having a compact code base makes it cheaper to make experimental changes.

* Can we use
  [`MonadAsync`](https://hackage.haskell.org/package/io-classes-1.4.1.0/docs/Control-Monad-Class-MonadAsync.html)
  and [IOSim](https://hackage.haskell.org/package/io-sim) to make parallel testing deterministic?

* Improving Random Generation
  + Generating Good Generators for Inductive Relations [POPL’18]
  + Beginner’s Luck [POPL’17]

* Incorporating Other Testing Techniques
  + Coverage Guided, Property Based Testing [OOPSLA’19]
  + Combinatorial Property-Based Testing: Do Judge a Test by its Cover [ESOP’21]

* Liveness a la quickcheck-dynamic?

* Distributed systems
  - Fault injection
    + [*Simple Testing Can Prevent Most Critical
      Failures*](https://www.usenix.org/conference/osdi14/technical-sessions/presentation/yuan)
      by Yuan et al (OSDI 2014)
    + Jepsen's knossos checker
  - Simulation testing
    + Always and sometimes combinators?

Formal specification and proofs are fundamental to computer science and have
occupied minds since [Alan
Turing](https://turingarchive.kings.cam.ac.uk/publications-lectures-and-talks-amtb/amt-b-8)
(1949). Property-based testing gives us an execellent opportunity to introduce
formal specification to a lot of programmers without the tedious and laborious
formal proof part, we should cherish such eduction opportunities.

[^1]: Is there a source for this story? I can't remember where I've heard it.
    This short
    [biography](http://www.erlang-factory.com/conference/London2011/speakers/JohnHughes)
    gives some of the details:

    > From 2002-2005 he led a major research project in software verification,
    > funded by the Swedish Strategic Research Foundation. This led to the
    > development of Quviq QuickCheck in Erlang.

    I believe [this](https://strategiska.se/forskning/genomford-forskning/ramanslag-inom-it-omradet/projekt/2010/)
    must be the project mentioned above.

[^2]: There's some room for error here from the users side, e.g. the user could
    create non-unique refererences. In a proper library one might want to
    introduce a `genSym` construct which guarantees uniqueness.
