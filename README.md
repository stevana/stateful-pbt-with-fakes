# The sad state of property-based testing libraries

Property-based testing is a rare example of academic research that has made it
to the mainstream in less than 30 years.

Under the slogan "don't write tests, generate them" property-based testing has
gained support from a diverse group of programming language communities.

In fact, the [Wikipedia](https://en.wikipedia.org/wiki/QuickCheck) page for the
original Haskell QuickCheck implementation lists 57 reimplementations in other
languages.

In this post I'd like to survey the most popular property-based testing
implementations and compare them with the state-of-the-art.

As the title already gives away, most of the libraries do not offer their users
the most advanced property-based testing features.

In order to best explain what's missing and why I think we ended up in this
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
"off".

So, just for fun, he started experimenting with the idea of testing if a program
respects a formal specification. Typically in dependently typed programming you
use the types to write the specification and then the program that implements
that type is the formal proof that the program is correct.

For example, let's say you've implemented a list sorting function, the
specification typically then is that the output of the sorting function is
ordered, i.e. for any index $i$ in your output list the element at that index
must be smaller or equal to the element at index $i + 1$.

Formally proving that a program is correct with respect to a specification is
often as much work as writing the program in the first place, so merely testing
it can often be a sweet spot where you get some confidence that the
specification is correct, without having to do the proving work. For example in
the sorting example you can simply compare the output of your sorting function
with the one in the standard library (which is very likely to be correct).

As programs get more complicted the ratio of effort saved by merely testing, as
opposed to proving, increases. In fact for bigger programs the effort involved
in proving correctness is simply too high for it to be practical (this is an
active area of research). Given all this, I hope you can at least start to see
why this idea excited John.

While John was working on this idea, Koen Claessen, another member of the
Functional Programing group, [stuck his
head](https://youtu.be/x4BNj7mVTkw?t=289) into John's office and asked what he
was doing. Koen too quickly got excited and came back the next day with his
improved version of John's code. There was some things that Koen hadn't thought
about, so John iterated on his code and so it went back and forth for a week
until the first implementation of property-based testing was written and not
long after they publised the paper [*QuickCheck: A Lightweight Tool for Random
Testing of Haskell
Programs*](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)
(ICFP 2000).

I think it's worth stressing the *lightweight tool* part from the paper's title,
the complete source code for the [first
version](https://github.com/Rewbert/quickcheck-v1) of the library is included in
the appendix of the paper and it's about 300 lines of code.

Haskell and dependently typed programming languages, like Agda, are pure
functional programming languages, meaning that it's possible at the type-level
to distinguish whether a function has side-effects or not.

Proofs about functions in Agda, and similar languages, are almost always only
dealing with pure functions.

Probably as a result of this, the first version of QuickCheck can only test
pure functions. This shortcoming was rectified in the follow up paper [*Testing
monadic code with
QuickCheck*](https://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps) (2002) by
the same authors.

It's an important extension as it allows us to reason about functions that use
mutable state, file I/O and networking. It also lays the foundation for being
able to test concurrent programs, as we shall see below.

Around the same time (2002) John Hughes was applying for a major grant at the
Swedish Strategic Research Foundation, part of this process involved pitching in
front of a panel of people from industry. Some person from
[Ericsson](https://en.wikipedia.org/wiki/Ericsson) was on this panel and they
were interested in QuickCheck. There was also a serial entrepreneur on the panel
and she encouraged John to start a company, and the Ericsson person agreed to be
a first customer, and so Quviq AB was founded in 2006[^1].

The first project at Ericsson that Quviq helped out testing was written in
Erlang. Unlike Haskell, Erlang is not a pure functional programming language and
on top of that there's concurrency everywhere. So even the second, monadic,
version of QuickCheck didn't turn out to be ergonomic enough for the job.

This is what motivated the closed source Quviq QuickCheck version written in
Erlang, first mentioned in the paper [Testing telecoms software with Quviq
QuickCheck](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=b268715b8c0bcebe53db857aa2d7a95fbb5c5dbf)
(2006).

The main features of the closed source version that, as we shall see, are still
not found in many open source versions are:

  1. Sequential *stateful* property-based testing using a state machine model;
  2. *Parallel* testing with race condition detection for free reusing the
    sequential state machine model.

We shall describe how these features work in detail later.

For now let's just note that *stateful* testing in its current form was first
mentioned in [QuickCheck testing for fun and
profit](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
(2007). This paper also mentions that it took John four iterations to get the
stateful testing design right, so while the 2006 paper does mention stateful
testing it's likely containing an earlier version of it.

While the 2007 paper also mentiones *parallel* testing via traces and
interleavings, it's vague on details. It's only later in [Finding Race
Conditions in Erlang with QuickCheck and
PULSE](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
(ICFP 2009) that parallel testing is described in detail including a reference
to [Linearizability: a correctness condition for concurrent
objects](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf) which is the
main technique behind it.

I'd like to stress that no Quiviq QuickCheck library code is every shared in any
of these papers, they only contain the library APIs (which are public) and test
examples implemented using said APIs.

After that most papers are experience reports of applying Quviq QuickCheck at
different companies, e.g. *Testing A Database for Race Conditions with
QuickCheck* (2011), *[Testing the hard stuff and staying
sane](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)*
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
how well supported these two testing features are.

Let me be clear up front that I've not used all of these libraries. My
understanding comes from reading the documentation, issue tracker and sometimes
source code.

To my best knowledge, as of April 2024, the following table summarises the
situation. Please open an
[issue](https://github.com/stevana/stateful-pbt-with-fakes/issues), PR, or get
in [touch](https://stevana.github.io/about.html) if you see a mistake or an
important omission.

| Library | Language | Stateful | Parallel | Notes |
| :---    | :---     | :---:    | :---:    | :---  |
| ScalaCheck | Scala | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | Has some support for parallel testing, but it's limited as can be witnessed by the fact that the two [examples](https://github.com/typelevel/scalacheck/tree/19af6eb656ba759980664e29ec6ae3e063021685/examples) of testing LevelDB and Redis both are sequential (`threadCount = 1`). |
| Gopter | Go | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | The README says "No parallel commands ... yet?" and there's an open [issue](https://github.com/leanovate/gopter/issues/20) from 2017. |
| Rapid | Go | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | |
| Hypothesis | Python | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | |
| PropEr | Erlang | <ul><li>- [x] </li></ul> | <ul><li>- [x] </li></ul> | First open source library to support both? |
| quickcheck | Rust | <ul><li>- [ ] </li></ul> | <ul><li> - [ ] </li></ul> | Issue to add stateful testing has been [closed](https://github.com/BurntSushi/quickcheck/issues/134). |
| proptest-state-machine | Rust | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | Documentation says "Currently, only sequential strategy is supported, but a concurrent strategy is planned to be added at later point.". |
| rantly | Ruby | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | |
| jsverify | JavaScript | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | There's an open [issue](https://github.com/jsverify/jsverify/issues/148) from 2015. |
| fast-check | TypeScript | <ul><li>- [x] </li></ul> | <ul><li>- [x] </li></ul> | Has [some support](https://fast-check.dev/docs/advanced/race-conditions/) for race condition checking of stateful programs, it's not clear to me how it relates to Quviq's Erlang QuickcCheck's parallel testing though. |
| SwiftCheck | Swift | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | There's an open [issue](https://github.com/typelift/SwiftCheck/issues/149) from 2016. |
| propcheck | Elixir | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | There's an open [issue](https://github.com/alfert/propcheck/issues/148) from 2020. |
| jetCheck | Java | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | From the source code "Represents an action with potential side effects, for single-threaded property-based testing of stateful systems.". |
| QuickTheories | Java | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | Has [experimental](https://github.com/quicktheories/QuickTheories/issues/42) for stateful testing, there's also some parallel testing, but it's inefficient and restrictive compared to QuviQ's Erlang version of QuickCheck. From the [source code](https://github.com/quicktheories/QuickTheories/blob/a963eded0604ab9fe1950611a64807851d790c1c/core/src/main/java/org/quicktheories/core/stateful/Parallel.java#L35): "Supplied commands will first be run in sequence and compared against the model, then run concurrently. All possible valid end states of the system will be calculated, then the actual end state compared to this. As the number of possible end states increases rapidly with the number of commands, command lists should usually be constrained to 10 or less." |
| FsCheck | F# | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | Has experimental [stateful testing](https://fscheck.github.io/FsCheck//StatefulTestingNew.html). An [issue](https://github.com/fscheck/FsCheck/issues/214) to add parallel support has been open since 2016. |
| test.check | Clojure | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | Someone has implemented stateful testing in a blog [post](http://blog.guillermowinkler.com/blog/2015/04/12/verifying-state-machine-behavior-using-test-dot-check/) though. |
| RapidCheck | C++ | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | There's an open [issue](https://github.com/emil-e/rapidcheck/issues/47) to add parallel support from 2015. |
| QuickCheck | Haskell | <ul><li>- [ ] </li></ul> | <ul><li>- [ ] </li></ul> | Open [issue](https://github.com/nick8325/quickcheck/issues/139) since 2016. |
| Hedgehog | Haskell | <ul><li>- [x] </li></ul> | <ul><li>- [x] </li></ul> | Has parallel support, but the implementation has [issues](https://github.com/hedgehogqa/haskell-hedgehog/issues/104). |
| quickcheck-state-machine | Haskell | <ul><li>- [x] </li></ul> | <ul><li>- [x] </li></ul> | Second open source library with parallel testing support? (I was [involved](https://github.com/nick8325/quickcheck/issues/139#issuecomment-272439099) in the development.) |

## Analysis

I hope that by now I've managed to convince you that most property-based testing
libraries do not implement the state-of-the-art when it comes to property-based
testing.

Many lack stateful testing via state machines (2007) and most lack parallel
testing support (2009).

Often users of the libraries have opened tickets asking for these features,
often these tickets have stayed open for years without any progress.

Why are property-based testing libraries in such a sad state?


1. Not as useful as testing pure functions? This is what John told me when I
   asked him why stateful and parallel testing hasn't taken off in Haskell
   (BobKonf 2017)
   + separate pure from side-effectful code is certainly good practice

   + however as witnessed by Quviq's very first stint into industry, stateful
     systems are everywhere there: databases, concurrent datastructures, etc

2. More difficult/work to model?
  + john hughes [says](https://youtu.be/x4BNj7mVTkw?t=898) testing this way
    requires a bit different way of thinking and you can't just give people the
    tool.

  + formal specification and proofs are fundamental to CS and have occupied
    minds since [Alan
    Turing](https://turingarchive.kings.cam.ac.uk/publications-lectures-and-talks-amtb/amt-b-8)
    (1949), here we have an execellent opportunity to introduce formal
    specification to a lot of programmers without the formal proof part...

3. No concise code to port?

  + Part of the original implementations spread to other languages can perhaps
    be attributed to the fact that the original implementation is small, around
    300 lines of code?

> Thomas Arts and I have founded a start-up, Quviq AB, to develop and
> market Quviq QuickCheck. Interestingly, this is the second implementation of
> QuickCheck for Erlang. The first was presented at the Erlang User Conference
> in 2003, and made available on the web. Despite enthusiasm at the conference, it
> was never adopted in industry. We tried to give away the technology, and it didn’t
> work! So now we are selling it, with considerably more success. Of course, Quviq
> QuickCheck is no longer the same product that was offered in 2003—it has been
> improved in many ways, adapted in the light of customers’ experience, extended
> to be simpler to apply to customers’ problems, and is available together with
> training courses and consultancy. That is, we are putting a great deal of work
> into helping customers adopt the technology. It was naive to expect that simply
> putting source code on the web would suffice to make that happen, and it would
> also be unreasonable to expect funding agencies to pay for all the work involved.
> In that light, starting a company is a natural way for a researcher to make an
> impact on industrial practice—and so far, at least, it seems to be succeeding.
Source: John Hughes in [QuickCheck testing for fun and profit](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c) (2007)

  + Trying to replicate the results from the Quviq QuickCheck papers (from 2006
    and onwards) without buying a Quviq QuickCheck license, is almost impossible
    without a lot of reverse engineering work.



* state machine testing gets a [bad
  reputation](https://lobste.rs/s/1aamnj/property_testing_stateful_code_rust#c_jjs27f)
  for being hard to learn and heavyweight


## Synthesis

In the rest of this post:

  1. show how one can implement stateful property-based testing in 150 lines of code.
     This is the first step

  2. add parallel testing in ~300 lines of code

  3. put this technique in context of software development at large.


### QuickCheck recap (stateless property-based testing)

The original idea is that we can test some pure (or side-effect free) function
$f : A \to B$ by randomly generating its argument ($A$) and then checking that
some predicate ($P : B \to Bool$) on the output holds.

For example let's say that the function we want to test is a list reversal
function ($reverse$), then the argument we need to randomly generate is a list,
and the predicate can be anything we'd like to hold for our list reversal
function, for example we can specify that reversing the result of rerversal
gives back the original list, i.e. $reverse(reverse(xs)) \equiv xs$.

* Involution, and other common properties


Before we get into how to apply property-based testing (PBT) to stateful
systems, lets recall what PBT of pure programs looks like. Here are a few
typical examples:

- `forall (xs : List Int). reverse (reverse xs) == xs`
- `forall (i : Input). deserialise (serialise i) == i`
- `forall (xs : List Int). sort (sort xs) == sort xs`
- `forall (i j k : Int). (i + j) + k == i + (j + k)`
- `forall (x : Int, xs : List Int). member x (insert x xs) && not (member x (remove x xs))`

The idea is that we quantify over some inputs (left-hand side of the `.` above)
which the PBT library will instantiate to random values before checking the
property (right-hand side). In effect the PBT library will generate unit tests,
e.g. the list `[1, 2, 3]` can be generated and reversing that list twice will
give back the same list. How many unit tests are generated can be controlled via
a parameter of the PBT library.

Typical properties to check for include: involution (reverse example above),
inverses (serialise example), idempotency (sort example), associativity
(addition example), axioms of abstract datatypes (member example) etc. Readers
familiar with discrete math might also notice the structural similarity of PBT
with proof by induction, in a sense: the more unit tests we generate the closer
we come to approximating proof by induction (not quite true but could be a
helpful analogy for now, we'll come back to this later).

* Most tutorials on property-based testing only cover testing pure functions

### Stateful property-based testing in ~150 LOC

* John's MGS course and quickcheck-dynamic
  + https://www.cse.chalmers.se/~rjmh/MGS2019/
  - still no parallel testing
* Edsko's lockstep https://www.well-typed.com/blog/2019/01/qsm-in-depth/

### Parallel property-based testing in ~300 LOC

* Condensed version of qsm's parallel testing built upon the above

### Contract tested fakes

* Edsko's lockstep https://www.well-typed.com/blog/2019/01/qsm-in-depth/

## Future work

Having a compact code base makes it cheaper to make experimental changes.

* Translate code to other programming language paradigms, thus making it easier
  for library implementors

* Can we use
  [`MonadAsync`](https://hackage.haskell.org/package/io-classes-1.4.1.0/docs/Control-Monad-Class-MonadAsync.html)
  and [IOSim](https://hackage.haskell.org/package/io-sim) to make parallel testing deterministic?

* Distributed systems
  - Fault injection
    + Jepsen's knossos checker
  - Simulation testing
    + Always and sometimes combinators?


## See also


* https://github.com/nick8325/quickcheck/issues/139

* [Experiences with QuickCheck: Testing the Hard Stuff and Staying
  Sane](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quviq-testing.pdf)
  - https://www.youtube.com/watch?v=zi0rHwfiX1Q

* [How to specify it! A Guide to Writing Properties of Pure
  Functions](https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf) (2020)
  + https://www.youtube.com/watch?v=zvRAyq5wj38

* [Building on developers' intuitions to create effective property-based
  tests](https://www.youtube.com/watch?v=NcJOiQlzlXQ)


[^1]: Is there a source for this story? I can't remember where I've heard it.
    This short
    [biography](http://www.erlang-factory.com/conference/London2011/speakers/JohnHughes)
    gives some of the details:

    > From 2002-2005 he led a major research project in software verification,
    > funded by the Swedish Strategic Research Foundation. This led to the
    > development of Quviq QuickCheck in Erlang.

    I believe [this](https://strategiska.se/forskning/genomford-forskning/ramanslag-inom-it-omradet/projekt/2010/)
    must be the project mentioned above.
