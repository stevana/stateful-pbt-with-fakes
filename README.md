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

Having recalled how pure property-based testing works, with its input
generation, shrinking and properties, let's now build a module on top which
allows us to do stateful testing.

Before we do so, I'll give some reasons why developing such a module make sense,
how it works at a high-level and what my sources of inspiration are.

#### Motivation

In order to explain why stateful property-based testing is needed, it's perhaps
helpful to have a look at how it's different from pure property-based testing.

A pure, or side-effect free, function always returns the same output if given
the same input. This isn't true for a stateful function or component.

For example, one of the simplest stateful components we can build is a counter.
If we think of the counter as a black box and our input is "increment the
counter" and the output is the new value, then repeating the same input will not
give the same output (assuming "increment the counter" is implemented using
$+1$).

So one difference is that we need to account for the state of the counter, or
the history of all inputs, while testing.

Another difference is that stateful systems often create some sort of reference
or handle, which allows the user to operate on some resource. For example when
working with a POSIX-like filesystem we open, operate on and finally close file
handles. Or some HTTP API might return a UUID to represent some page and
subsequent changes to that page via API calls need to include that UUID, etc.

So while it's certainly possible to test stateful systems using vanilla
property-based testing, it's convenient to have a library take care of modelling
the state and dealing with references.

Furthermore, parallel testing builds upon the stateful testing interface without
requiring the user to do any extra work, but more on this later.

#### How it works

The high-level idea behind stateful property-based testing is that first we
create some kind of in-memory model of the system we want to test.

For example in the case of the counter the model is just an integer, while in
the case of the filesystem a simplified model is a tree with filepaths as node
labels and children either being other trees (directories) or leaves of bytes
(files).

Once we have our model we generate sequences of inputs, where inputs are actions
the user can take, e.g. increment in the case of a counter or
open/read/write/close in the case of filesystem.

The generated sequence of inputs is then executed against the real component as
well as the model of it, and then the outputs are compared to check that the
real component is faithful to the model. If it isn't we shrink the sequence of
inputs to try to present the minimal counterexample, as in the stateless case.

#### Prior work

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

I'll refer back to these when I motivate my design decisions below.

#### Implementation

There are three parts to the implementation. First the stateful testing
interface (or type class), this is what the user needs to implement, the rest of
the library is programmed against this interface and provides the testing
functionality. The second part is generating and shrinking sequences of inputs,
which is derived from the interfaces ability to generate and shrink individual
inputs. The third and final part is about executing the generated inputs against
the real component and the model as well as checking that they conform.

##### Stateful testing interface

The following is the interface which the user of the library has to implement in
order for the library to provide the stateful property-based testing
functionality.

Don't try to make sense of all of this on a first read. There will be plenty of
examples that will hopefully help make things more concrete as we go along.

```haskell
```

##### Generating and shrinking

```haskell
```

##### Running and assertion checking

```haskell
```

#### Example: counter

To make things more concrete, let's have a look at an example. All examples, in
the rest of this post, will have three parts:

  1. The software under test (SUT);
  2. The model or fake that the SUT gets tested against;
  3. The generated tests and output from running them.

The SUT in this example is a counter implemented using a mutable variable.

##### SUT

They don't teach you this in school, but this is how you can implement global
mutable variables in Haskell:

```haskell
gLOBAL_COUNTER :: IORef Int
gLOBAL_COUNTER = unsafePerformIO (newIORef 0)
{-# NOINLINE gLOBAL_COUNTER #-}

incr :: IO ()
incr :: IO ()
incr = do
  n <- readIORef gLOBAL_COUNTER
  writeIORef gLOBAL_COUNTER (n + 1)

get :: IO Int
get = readIORef gLOBAL_COUNTER
```

The reason it's not taught is because global state easily leads to a mess, but
in our case it provides the minimal possible example of a stateful system.

##### Model

To model our counter, we'll implement a fake of a counter using an integer.

```haskell
newtype Counter = Counter Int
  deriving (Eq, Show)

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

  -- We'll generate increments and reads of the counter with equal probability.
  generateCommand :: Counter -> Gen (Command Counter r)
  generateCommand _s = elements [Incr, Get]

  -- The fake takes a command and the model of the counter and returns a new
  -- model and a response.
  runFake :: Command Counter r -> Counter -> Either Void (Counter, Response Counter r)
  runFake Incr  (Counter n) = return (Counter (n + 1), Incr_ ())
  runFake Get m@(Counter n) = return (m, Get_ n)

  -- We also need to explain which part of the counter API each command
  -- corresponds to.
  runReal :: Command Counter r -> IO (Response Counter r)
  runReal Get  = Get_  <$> get
  runReal Incr = Incr_ <$> incr
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
incr42Bug = modifyIORef' gLOBAL_COUNTER
  (\n -> if n == 42 then n else n + 1)
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

#### Example: array-based queue

The next example is an array-based queue written in C. This example is taken
from John's paper [*Testing the hard stuff and staying
sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
(2014).

##### SUT

The implementation uses two indices which keep track of the front and back of
the queue, this allows us to implement the queue in a circular buffer fashion.
I've copied the C code straight from the paper and it contains a subtle bug, can
you spot it?

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

In the counter example above we only had one counter, so the model was merely a
single integer. In this example, because of `new` returning a queue, we need to
be able to model arbitrary many queues. We can this as follows:

```haskell
type State = Map (Var Queue) FQueue

data Var a = Var Int
```

Where `Queue` is the Haskell data type that corresponds to the C `Queue` and the
`Var a` data type is provided by the library and is a symbolic reference to `a`
(just an `Int`eger). The idea being that in the model we don't have access to
real `Queue`s, merely symbolic references to them. This might seem a bit
strange, but I hope that it will become more clear when we model `new`.

```haskell
```

```haskell
data Err = QueueDoesNotExist | QueueIsFull | QueueIsEmpty
  deriving (Eq, Show)


fnew :: Int -> State -> Either Err (State, Var Queue)
fnew sz s =
  let
    v = Var (Map.size s)
  in
    return (Map.insert v (FQueue [] sz) s, v)

fput :: Var Queue -> Int -> State -> Either Err (State, ())
fput q i s
  | q `Map.notMember` s = Left QueueDoesNotExist
  | length (fqElems (s Map.! q)) >= fqSize (s Map.! q) = Left QueueIsFull
  | otherwise = return (Map.adjust (\fq -> fq { fqElems = fqElems fq ++ [i] }) q s, ())

fget :: Var Queue -> State -> Either Err (State, Int)
fget q s
  | q `Map.notMember` s        = Left QueueDoesNotExist
  | null (fqElems (s Map.! q)) = Left QueueIsEmpty
  | otherwise = case fqElems (s Map.! q) of
      [] -> error "fget: impossible, we checked that it's non-empty"
      i : is -> return (Map.adjust (\fq -> fq { fqElems = is }) q s, i)

fsize :: Var Queue -> State -> Either Err (State, Int)
fsize q s
  | q `Map.notMember` s = Left QueueDoesNotExist
  | otherwise           = return (s, length (fqElems (s Map.! q)))
```

```haskell
instance StateModel State where

  initialState = Map.empty

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
      , Size <$> arbitraryQueue
      ]
    where
      arbitraryQueue :: Gen (Var Queue)
      arbitraryQueue = Var <$> choose (0, Map.size s - 1)

  shrinkCommand _s (Put q i) = [ Put q i' | i' <- shrink i ]
  shrinkCommand _s _cmd = []

  runFake (New sz)  s = fmap New_  <$> fnew sz s
  runFake (Put q i) s = fmap Put_  <$> fput q i s
  runFake (Get q)   s = fmap Get_  <$> fget q s
  runFake (Size q)  s = fmap Size_ <$> fsize q s

  runReal (New sz)  = New_  <$> new sz
  runReal (Put q i) = Put_  <$> put q i
  runReal (Get q)   = Get_  <$> get q
  runReal (Size q)  = Size_ <$> size q
```

##### Testing

```haskell
prop_queue :: Commands State -> Property
prop_queue cmds = monadicIO $ do
  runCommands cmds
  assert True
```

+ regression tests?

#### Example: process registry

This example comes from the paper [*QuickCheck testing for fun and
profit*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
(2007) and is also part of John's Midlands Graduate School course (2019).

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

  runReal :: Concrete Model -> IO (Response Model (Reference Model))
  runReal _cmd = return Done

  monitoring :: (Model, Model) -> Concrete Model -> Response Model (Reference Model)
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

#### Example: parallel process registry

The parallel tests for the process registry was introduced in [*Finding Race
Conditions in Erlang with QuickCheck and
PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
(2009)

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
