# The sad state of property-based testing libraries

*Work in progress, please don't share, but do get involved!*

Property-based testing is a rare example of academic research that has
made it to the mainstream in less than 30 years. Under the slogan "don't
write tests, generate them" property-based testing has gained support
from a diverse group of programming language communities. In fact, the
Wikipedia page of the original property-basted testing Haskell library,
[QuickCheck](https://en.wikipedia.org/wiki/QuickCheck), lists 57
reimplementations in other languages.

In this post I'd like to survey the most popular property-based testing
implementations and compare them with what used to be the
state-of-the-art fifteen years ago (2009). As the title already gives
away, most of the libraries do not offer their users the most advanced
property-based testing features. In order to best explain what's missing
and why I think we ended up in this situation, let me start by telling
the brief history of property-based testing.

## The history of property-based testing

In Gothenburg, Sweden's second most populated city, there's a university
called Chalmers. At the computer science department of Chalmers there
are several research groups, two of which are particularly relevant to
our story -- the *Functional Programming* group and *Programming Logic*
group. I'll let you guess what the former group's main interest is. The
latter group's mostly conserned with a branch of functional programming
where the type system is sufficiently expressive that it allows for
formal specifications of programs, sometimes called dependently typed
programming or type theory. Agda is an example of a Haskell-like
dependently typed programming language, that also happens to be mainly
developed by the Programing Logic group. Given the overlap of interest
and proximity, researchers at the department are sometimes part of both
groups or at least visit each others research seminars from time to
time.

John Hughes is a long-time member of the Functional Programming group,
who's also well aware of the research on dependently typed programming
going on in the Programming Logic group. One day in the late nineties,
after having worked hard on finishing something important on time, John
found himself having a week "off". So, just for fun, he started
experimenting with the idea of testing if a program respects a formal
specification.

Typically in dependently typed programming you use the types to write
the specification and then the program that implements that type is the
formal proof that the program is correct. For example, let's say you've
implemented a list sorting function, the specification typically then is
that the output of the sorting function is ordered, i.e. for any index
$i$ in your output list the element at that index must be smaller or
equal to the element at index $i + 1$. Formally proving that a program
is correct with respect to a specification is often as much work as
writing the program in the first place, so merely testing it can often
be a sweet spot where you get some confidence that the specification is
correct, without having to do the proving work. For example in the
sorting example you can simply generate a random input list and then
compare the output of your sorting function with the one in the standard
library (which is likely to be correct). As programs get more complicted
the ratio of effort saved by merely testing, as opposed to proving,
increases. In fact for bigger programs the effort involved in proving
correctness is simply too high for it to be practical (this is an active
area of research). Given all this, I hope that you can start to see why
this idea excited John.

While John was working on this idea, Koen Claessen, another member of
the Functional Programing group, [stuck his
head](https://youtu.be/x4BNj7mVTkw?t=289) into John's office and asked
what he was doing. Koen got excited as well and came back the next day
with his improved version of John's code. There was some things that
Koen hadn't thought about, so John iterated on his code and so it went
back and forth for a week until the first implementation of
property-based testing was written and not long afterwards they publised
the paper [*QuickCheck: A Lightweight Tool for Random Testing of Haskell
Programs*](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)
(ICFP 2000). I think it's worth stressing the *lightweight tool* part
from the paper's title, the complete source code for the [first
version](https://github.com/Rewbert/quickcheck-v1) of the library is
included in the appendix of the paper and it's about 300 lines of code.

Haskell and dependently typed programming languages, like Agda, are pure
functional programming languages, meaning that it's possible at the
type-level to distinguish whether a function has side-effects or not.
Proofs about functions in Agda, and similar languages, are almost always
only dealing with pure functions. Probably as a result of this, the
first version of QuickCheck can only test pure functions. This
shortcoming was rectified in the follow up paper [*Testing monadic code
with
QuickCheck*](https://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps)
(2002) by the same authors. It's an important extension as it allows us
to reason about functions that use mutable state, file I/O and
networking, etc. It also lays the foundation for being able to test
concurrent programs, as we shall see below.

Around the same time as the second paper was published (2002), John was
applying for a major grant at the Swedish Strategic Research Foundation.
A part of the application process involved pitching in front of a panel
of people from industry. Some person from
[Ericsson](https://en.wikipedia.org/wiki/Ericsson) was on the panel and
they were interested in QuickCheck. There was also a serial entrepreneur
on the panel and she encouraged John to start a company, and the
Ericsson person agreed to be a first customer, and so Quviq AB was
founded in 2006[^1] by John and Thomas Arts (perhaps somewhat
surprisingly, Koen was never involved in the company).

The first project at Ericsson that Quviq helped out testing was written
in Erlang. Unlike Haskell, Erlang is not a pure functional programming
language and on top of that there's concurrency everywhere. So even the
second, monadic, version of QuickCheck didn't turn out to be ergonomic
enough for the job. This is what motivated the closed source Quviq
QuickCheck version written in Erlang, first mentioned in the paper
[*Testing telecoms software with Quviq
QuickCheck*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=b268715b8c0bcebe53db857aa2d7a95fbb5c5dbf)
(2006). The main features of the closed source version that, as we shall
see, are still not found in many open source versions are:

1.  Sequential *stateful* property-based testing using a state machine
    model;
2.  *Parallel* testing with race condition detection by reusing the
    sequential state machine model.

We shall describe how these features work in detail later. For now let's
just note that *stateful* testing in its current form was first
mentioned in [*QuickCheck testing for fun and
profit*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
(2007). This paper also mentions that it took John four iterations to
get the stateful testing design right, so while the 2006 paper already
does mention stateful testing it's likely containing one of those
earlier iteration of it.

While the 2007 paper also mentiones *parallel* testing via traces and
interleavings, it's vague on details. It's only later in [*Finding Race
Conditions in Erlang with QuickCheck and
PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
(ICFP 2009) that parallel testing is described in detail including a
reference to [*Linearizability: a correctness condition for concurrent
objects*](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf) by
Herlihy and Wing (1990) which is the main technique behind it.

I'd like to stress that no Quviq QuickCheck library code is ever shared
in any of these papers, they only contain the library APIs (which are
public) and test examples implemented using said APIs.

After that most papers are experience reports of applying Quviq
QuickCheck at different companies, e.g. *Testing A Database for Race
Conditions with QuickCheck* (2011), [*Testing the hard stuff and staying
sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
(2014), *Testing AUTOSAR software with QuickCheck* (2015), *Mysteries of
Dropbox: Property-Based Testing of a Distributed Synchronization
Service* (2016).

Sometimes various minor extenions to stateful and parallel testings are
needed in order to test some particular piece of software, e.g. C FFI
bindings in the case of AUTOSAR or eventual consistency in the case of
Dropbox, but by and large the stateful and parallel testing features
remain the same.

## A survey of property-based testing libraries

As we've seen above, the current state-of-the-art when it comes to
property-based testing is *stateful* testing via a state machine model
and reusing the same sequential state machine model combined with
linearisability to achieve *parallel* testing.

Next let's survey the most commonly used property-based testing
libraries to see how well supported these two testing features are. Let
me be clear up front that I've not used all of these libraries. My
understanding comes from reading the documentation, issue tracker and
sometimes source code.

To my best knowledge, as of June 2024, the following table summarises
the situation. Please open an
[issue](https://github.com/stevana/stateful-pbt-with-fakes/issues), PR,
or get in [touch](https://stevana.github.io/about.html) if you see a
mistake or an important omission.

| Library                  | Language   | Stateful | Parallel | Notes                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|:-------------------------|:-----------|:--------:|:--------:|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Eris                     | PHP        |    ☐     |    ☐     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| FsCheck                  | F#         |    ☒     |    ☐     | Has experimental [stateful testing](https://fscheck.github.io/FsCheck//StatefulTestingNew.html). An [issue](https://github.com/fscheck/FsCheck/issues/214) to add parallel support has been open since 2016.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Gopter                   | Go         |    ☒     |    ☐     | The README says "No parallel commands ... yet?" and there's an open [issue](https://github.com/leanovate/gopter/issues/20) from 2017.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Hedgehog                 | Haskell    |    ☒     |    ☒     | Has parallel support, but the implementation has [issues](https://github.com/hedgehogqa/haskell-hedgehog/issues/104).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Hypothesis               | Python     |    ☒     |    ☐     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| PropEr                   | Erlang     |    ☒     |    ☒     | First open source library to support both?                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| QuickCheck               | Haskell    |    ☐     |    ☐     | There's an open [issue](https://github.com/nick8325/quickcheck/issues/139) to add stateful testing since 2016.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| QuickTheories            | Java       |    ☒     |    ☐     | Has [experimental](https://github.com/quicktheories/QuickTheories/issues/42) for stateful testing, there's also some parallel testing, but it's inefficient and restrictive compared to QuviQ's Erlang version of QuickCheck. From the [source code](https://github.com/quicktheories/QuickTheories/blob/a963eded0604ab9fe1950611a64807851d790c1c/core/src/main/java/org/quicktheories/core/stateful/Parallel.java#L35): "Supplied commands will first be run in sequence and compared against the model, then run concurrently. All possible valid end states of the system will be calculated, then the actual end state compared to this. As the number of possible end states increases rapidly with the number of commands, command lists should usually be constrained to 10 or less." |
| Rapid                    | Go         |    ☒     |    ☐     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| RapidCheck               | C++        |    ☒     |    ☐     | There's an open [issue](https://github.com/emil-e/rapidcheck/issues/47) to add parallel support from 2015.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| ScalaCheck               | Scala      |    ☒     |    ☐     | Has some support for parallel testing, but it's limited as can be witnessed by the fact that the two [examples](https://github.com/typelevel/scalacheck/tree/19af6eb656ba759980664e29ec6ae3e063021685/examples) of testing LevelDB and Redis both are sequential (`threadCount = 1`).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| SwiftCheck               | Swift      |    ☐     |    ☐     | There's an open [issue](https://github.com/typelift/SwiftCheck/issues/149) to add stateful testing from 2016.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| fast-check               | TypeScript |    ☒     |    ☐     | Has [some support](https://fast-check.dev/docs/advanced/race-conditions/) for race condition checking, but it seems different from Quviq QuickCheck's parallel testing. In particular it doesn't seem to reuse the sequential state machine model nor use linearisability.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| jetCheck                 | Java       |    ☒     |    ☐     | From the source code "Represents an action with potential side effects, for single-threaded property-based testing of stateful systems.".                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| jsverify                 | JavaScript |    ☐     |    ☐     | There's an open [issue](https://github.com/jsverify/jsverify/issues/148) to add stateful testing from 2015.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| lua-quickcheck           | Lua        |    ☒     |    ☐     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| propcheck                | Elixir     |    ☒     |    ☐     | There's an open [issue](https://github.com/alfert/propcheck/issues/148) to add parallel testing from 2020.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| proptest                 | Rust       |    ☐     |    ☐     | See proptest-state-machine.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| proptest-state-machine   | Rust       |    ☒     |    ☐     | Documentation says "Currently, only sequential strategy is supported, but a concurrent strategy is planned to be added at later point.".                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| qcheck-stm               | OCaml      |    ☒     |    ☒     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| quickcheck               | Prolog     |    ☐     |    ☐     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| quickcheck               | Rust       |    ☐     |    ☐     | Issue to add stateful testing has been [closed](https://github.com/BurntSushi/quickcheck/issues/134).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| quickcheck-state-machine | Haskell    |    ☒     |    ☒     | Second open source library with parallel testing support? (I was [involved](https://github.com/nick8325/quickcheck/issues/139#issuecomment-272439099) in the development.)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| rackcheck                | Racket     |    ☐     |    ☐     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| rantly                   | Ruby       |    ☐     |    ☐     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| test.check               | Clojure    |    ☐     |    ☐     | Someone has implemented stateful testing in a blog [post](http://blog.guillermowinkler.com/blog/2015/04/12/verifying-state-machine-behavior-using-test-dot-check/) though.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| theft                    | C          |    ☐     |    ☐     |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |

## Analysis

By now I hope that I've managed to convince you that most property-based
testing libraries do not implement what used to be the state-of-the-art
fifteen years ago.

Many libraries lack stateful testing via state machines and most lack
parallel testing support. Often users of the libraries have opened
tickets asking for these features, but the tickets have stayed open for
years without any progress. Furthermore it's not clear to me whether all
libraries that support stateful testing can be generalised to parallel
testing without a substantial redesign of their APIs. I don't think
there's a single example of a library to which parallel testing was
added later, rather than designed for from the start.

### Why are property-based testing libraries in such a sad state?

Here are three reasons I've heard from John:

1.  The stateful and parallel testing featurs are not as useful as
    testing pure functions. This is what John told me when I asked him
    why these features haven't taken off in the context of Haskell
    (BobKonf 2017);

2.  The state machine models that one needs to write for the stateful
    and parallel testing require a different way of thinking compared to
    normal testing. One can't merely give these tools to new users
    without also giving them proper training, John said in an
    [interview](https://youtu.be/x4BNj7mVTkw?t=898);

3.  Open source didn't work, a closed source product and associated
    services
    [helps](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
    adoption:

    > "Thomas Arts and I have founded a start-up, Quviq AB, to develop
    > and market Quviq QuickCheck. Interestingly, this is the second
    > implementation of QuickCheck for Erlang. The first was presented
    > at the Erlang User Conference in 2003, and made available on the
    > web. Despite enthusiasm at the conference, it was never adopted in
    > industry. We tried to give away the technology, and it didn’t
    > work! So now we are selling it, with considerably more success. Of
    > course, Quviq QuickCheck is no longer the same product that was
    > offered in 2003—it has been improved in many ways, adapted in the
    > light of customers’ experience, extended to be simpler to apply to
    > customers’ problems, and is available together with training
    > courses and consultancy. That is, we are putting a great deal of
    > work into helping customers adopt the technology. It was naive to
    > expect that simply putting source code on the web would suffice to
    > make that happen, and it would also be unreasonable to expect
    > funding agencies to pay for all the work involved. In that light,
    > starting a company is a natural way for a researcher to make an
    > impact on industrial practice—and so far, at least, it seems to be
    > succeeding."

A cynic might argue that there's a conflict of interest between doing
research and education on one hand and running a company that sells
licenses, training and consulting on the other.

Let me be clear that I've the utmost respect for John, and I believe
what he says to be true and I believe he acts with the best intentions.
Having said that let me try to address John's points.

#### Stateful and parallel testing isn't as useful as pure testing

I think many people will agree that separating pure from side-effectful
code is good practice in any programming language, and I do agree with
John that you can get far by merely property-based testing those pure
fragments.

However I also think that stateful and parallel testing is almost
equally important for many non-trivial software systems. Most systems in
industry will have some database, stateful protocol or use concurrent
data structures, which all benefit from the stateful and parallel
testing features.

#### Stateful modelling requires training

Regarding formal specification requiring a special way of thinking and
therefor training, I believe this is a correct assessment. However I
also believe that this is already true for property-based testing of
pure functions. A non-trained user of pure property-based testing will
likely test less interesting properties than someone who's trained.

Given that John has written
[papers](https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf)
and given [talks](https://www.youtube.com/watch?v=NcJOiQlzlXQ) on the
topic of making property-based testing of pure functions more accessible
to programmers, one might wonder why we cannot do the same for stateful
and parallel testing?

The experience reports, that we've mentioned above, usually contain some
novelty (which warrents publishing a new paper) rather than general
advice which can be done with the vanilla stateful and parallel testing
features. Furthermore they require buying a Quviq license in order to
reproduce the results, a show stopper for many people.

I think it's also worth stressing that stateful specifications are not
necessarily always more difficult than specifications for pure
functions. For example, to model a key-value store one can get quite far
with the model being a list of key-value pairs. In fact a simple model
like that managed to find a 17 step (shrunk) counterexample in LevelDB
to a known [issue](https://github.com/google/leveldb/issues/50), within
mere minutes. It took weeks for Google to provide a fix, and then after
running the property again on the fixed code a new 31 step
counterexample was found within minutes. Turns out there was a bug in
the background compaction process. The compaction process improves read
performance and reclaims disk space, which is important for a key-value
store, but interestingly it's not explicitly part of the model. Joseph W
Norton gave a
[talk](https://htmlpreview.github.io/?https://raw.githubusercontent.com/strangeloop/lambdajam2013/master/slides/Norton-QuickCheck.html)
at LambdaJam 2013 about it.

#### Closed source helps industry adoption

Regarding keeping the source closed helping with adoption, I think this
is perhaps the most controversial point that John makes.

If we try to see it from John's perspective, how else would an academic
get funding to work on tooling (which typically isn't reconginised as
doing research), or feedback from industry? Surely, one cannot expect
research funding agencies to pay for this?

On the other hand one could ask why there isn't a requirement that
published research should be reproducable using open source tools (or at
least tools that are freely available to the public and other
researchers)?

Trying to replicate the results from the Quviq QuickCheck papers (from
2006 and onwards) without buying a Quviq QuickCheck license, is almost
impossible without a lot of reverse engineering work.

I suppose one could argue that one could have built a business around an
open source tool, only charging for the training and consulting, but
given how broken open source is today, unless you are a big company
(which takes more than it gives back), it's definitely not clear that it
would have worked (and it was probably even worse back in 2006).

Even if John is right and that keeping it closed source has helped
adoption in industry, I think it's by now fair to say it has not helped
open source adoption.

Or perhaps rather, it's unlikely that a company that pays for a licence
in Erlang would then go and port the library in another language.

### What can we do about it?

I think there's at least two things worth trying.

1.  Provide a short open source implementation of stateful and parallel
    property-based testing, analogous to the original ~300LOC vanilla
    QuickCheck implementation.

    Perhaps part of the original QuickCheck library's success in
    spreading to so many other languages can be attributed to the fact
    that its small implementation and that it is part of the original
    paper?

2.  Try to make the formal specification part easier, so that we don't
    need to train developers (as much).

    Perhaps we can avoid state machines as basis for specifications and
    instead reuse concepts that programmers are already familiar with
    from their current testing activities, e.g. mocking and test doubles
    more generally?

## Synthesis

In order to test the above hypothesis, I'd like to spend the rest of
this post as follows:

1.  Show how one can implement stateful and parallel property-based
    testing in about 330 lines of code (similar to the size of the
    original QuickCheck implementation);

2.  Make specifications simpler by using
    [fakes](https://martinfowler.com/bliki/TestDouble.html) rather than
    state machines.

Before we get started with stateful testing, let's first recap how
property-based testing of pure functions works.

### Pure property-based testing recap

It's considered good practice to test new functions or functionality, to
make sure it does what we want. For example, imagine we've written a
linked-list reversal function called `reverse`, then it might be
sensible to test it against a couple of lists such as the empty list
and, say, the three element list `[1, 2, 3]`.

How does one choose which example inputs to test against though?
Typically one wants to choose corner cases, such as the empty list, that
perhaps were overlooked during the implementation. It's difficult to
think of corner cases that you might have overlooked! This is where
generating random inputs, a key feature of property-based testing, comes
in. The idea being that random inputs will eventually hit corner cases.

When we manually pick inputs for our tests, like `[1, 2, 3]`, we know
what the output should be and so we can make the appropriate assertion,
i.e. `reverse [1, 2, 3] == [3, 2, 1]`. When we generate random inputs we
don't always know what the output should be. This is where writing
properties that relate the output to the input somehow comes in. For
example, while we don't know what the output of reversing an arbitrary
list is, we do know that reversing it twice will give back the input.
This is how we can express this property in QuickCheck:

    >>> quickCheck (\(xs :: [Int]) -> reverse (reverse xs) == xs)
    +++ OK, passed 100 tests.

By default 100 tests get generated, but that can be adjusted:

    >>> quickCheck (withMaxSuccess 5 (\(xs :: [Int]) -> reverse (reverse xs) == xs))
    +++ OK, passed 5 tests.

We can see what test get generated using `verboseCheck`:

``` haskell
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

``` haskell
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

The list and integer generators are provided by the library and I hope
you agree that these seem like sensible arbitrary lists to use in our
tests.

Next let's have a look at when a property fails. For example this is
what happens if we try to test that the output of reversing a list is
the input list:

    >>> quickCheck (\(xs :: [Int]) -> reverse xs == xs)
    *** Failed! Falsified (after 3 tests and 2 shrinks):
    [0,1]

We see that after 3 tests a test case was generated that failed, the
input got shrunk twice and the minimal counter example `[0, 1]` is
presented. Notice that we do need a list that is at least of length two,
because any shorter list will reverse to itself.

As pointed out earlier, coming up with these properties is by no means
obvious. There are however a few patterns that come up over and over
again. With `reverse` we saw an example of an involutory function, i.e.
`f (f x) == x`, here are a few other examples:

- Inverses, e.g. `\(i :: Input) -> deserialise (serialise i) == i`;
- Idempotency, e.g. `\(xs :: [Int]) -> sort (sort xs) == sort xs`;
- Associativity, e.g. `\(i j k :: Int) -> (i + j) + k == i + (j + k)`;
- Axioms of abstract data types, e.g.
  `\(x :: Int)(xs :: [Int]) -> member x (insert x xs) && not (member x (remove x xs))`;
- Metamorphic properties, e.g.
  `\(g :: Graph)(m n :: Node) -> shortestPath g m n == shortestPath g n m`.

Readers familiar with discrete math might recognise some of the above.

### Stateful property-based testing in ~180 LOC

In the pure property-based testing case, that we just looked at, the
picture of the test setup looks a bit like this:

             +-----+
          i  |     |  o
        ----->  f  +---->
             |     |
             +-----+

Where `i` is the input we generate, `f` is the function we are applying
the generated input to to produce the output `o`. In the case of the
`reverse` example, from before, `i` and `o` are of type list of integers
(`[Int]`), `f` is `reverse . reverse` and the property that we check for
every generated input is that input is equal to the output.

Next let's contrast this picture with how the test setup looks when we
are testing a stateful component. A simple example of a stateful
component is a counter with an `incr`ement operation which increment the
counter and returns the old count.

Unlike in the pure case, the same input will not give the same output.
For example the first time we do `incr` we get back `0` (if we start
counting from zero) while the second time we do `incr` we get `1`. A
database or a file system are two other examples of stateful components,
where the history of previous inputs affects the output of the next
input.

In the stateful case, the picture looks more like this:

        +------+     +------+     +------+
        |      | i1  |      | i2  |      |
        |  s0  +----->  s1  +----->  s2  | ...
        |      |     |      |     |      |
        +------+     +--+---+     +--+---+
                        |            |
                        | o1         | o2
                        v            v

        ---------------------------------> time

Where `s` is the state, `i` is an input (e.g. `incr`) and `o` is an
ouput. Notice how the state evolves over time and depends on the history
of inputs.

In the pure case each test case is a single input, in the stateful case
we need a sequence of inputs in order to test how the system changes
over time. In the pure case our our properties were relations on the
input and output, i.e. `R : i -> o -> Bool`. In the stateful case our
properties would need to be generalised to `R' : [i] -> [o] -> Bool` to
account for how the state changes over time. Writing such properties is
cumbersome, an alternative is to account for the state explicitly by
means of some kind of model.

This model could be a state machine of type `m -> i -> (m, o)`, i.e. a
function from the old model and an input to the next model and the
output. From this we can derive a property that for each input checks if
the outputs of the stateful component agrees with the output of the
state machine:

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

In case the outputs disagree we shrink the sequence of inputs and try to
present the smallest counterexample, as in the pure case.

Let's make things more concrete with some actual code that we can run.

#### Example: counter

All examples, in the rest of this post, will have three parts:

1.  The software under test;
2.  The model that the software under test gets tested against;
3.  The generated tests and output from running them.

The first part is independent of the stateful testing library we are
building. The second part is hooking up the first part to the library by
implementing an interface (type class). We'll look at the definition of
the type class after the example. The final part is how to write the
actual property and interpret the output from running them.

##### Software under test

This is how you can implement a counter using a global mutable variable
in Haskell:

``` haskell
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

Notice that here `incr` doesn't return the old value, like above, and
instead we have a separate operation `get` which returns the current
value of the counter.

##### Model

To model our counter we'll use an integer.

``` haskell
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

A common complaint is that the model (`Counter` and `runFake`) is as big
as the implementation itself. This is true, because it's an example. In
reality the model will often be many orders of magnitude smaller. This
is due to the fact that the model, unlike the real implementation,
doesn't need to persisting to disk, communicating over the network, or
various perform time or space optimisations. Recall the LevelDB example
from above.

##### Tests

The tests, or property, can now be written as follows.

``` haskell
prop_counter :: Commands Counter -> Property
prop_counter cmds = monadicIO $ do
  runCommands cmds
  run reset
  assert True

reset :: IO ()
reset = writeIORef gLOBAL_COUNTER 0
```

To run them, we can load the module and type `quickCheck prop_counter`
in the REPL, which gives us an output like:

    +++ OK, passed 100 tests:
    89% Get
    85% Incr

    Commands (2151 in total):
    52.02% Get
    47.98% Incr

Where the first group of percentages tell us the proportion of tests
that contained the get and increment command respectively, and the
second group of percentages tell us the proportion of get and increment
commands out of all commands generated. Note that the first group
doesn't add up to 100%, because most tests will contain both commands,
whereas the second group does. The reason the second group is almost
50-50 is because in the generator we generate both commands with equal
probability.

Another thing to note is that we need to `reset` the counter between
tests, otherwise the global counter will have the value from the last
test while the model always starts from zero and we get a mismatch.

To make things a bit more interesting, let's introduce a bug into our
counter and see if the tests can find it. Let's make it so that if the
counter has the value of 42, then it won't increment properly.

``` haskell
incr42Bug :: IO ()
incr42Bug = do
  n <- readIORef gLOBAL_COUNTER
  let n' = if n == 42
           then n -- BUG
           else n + 1
  writeIORef gLOBAL_COUNTER n'
```

We also need to change the `runReal` function to use our buggy increment
as follows.

``` diff
- runReal Incr = Incr_ <$> incr
+ runReal Incr = Incr_ <$> incr42Bug
```

When we run the property now, we'll see something like the following
output.

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

Notice that this is indeed the smallest counterexample and how it took
66 randomly generated test cases to find the sequence of inputs that
triggered the bug and then 29 shrink steps for QuickCheck to minimise
it.

#### Stateful library implementation

In the example above we implemented the `StateModel` interface (or type
class), next we'll have a look at the definiton of this interface and
the testing functionality we can derive by programming against the
interface.

##### Stateful testing interface

Let me give you the full definition of the interface and then I'll
explain it in words afterwards.

``` haskell
class ( ...
```

``` haskell
      ) => StateModel state where

  -- If we think of the system under test as a black box, then commands are the
  -- inputs and responses the outputs to the black box.
  data Command  state :: Type -> Type
  data Response state :: Type -> Type

  -- Sometimes a command needs to refer to a previous response, e.g. when a file
  -- is opened we get a handle which is later refered to when writing or reading
  -- form the file. File handles, and similar constructs, are called references
  -- and can be part of commands and responses.
  type Reference state :: Type
  type Reference state = Void

  -- Not all commands are valid in all states. Pre-conditions allow the user to
  -- specify when a command is safe to execute, for example we cannot write or
  -- read to or from an unopened file. The `PreconditionFailure` data type
  -- allows the user to create custom pre-condition failures. By default now
  -- pre-condition failures are allowed, thus the `Void` (empty) type.
  type PreconditionFailure state :: Type
  type PreconditionFailure state = Void


  generateCommand :: state -> Gen (Command state (Var (Reference state)))

  shrinkCommand :: state -> Command state (Var (Reference state))
                -> [Command state (Var (Reference state))]
  shrinkCommand _state _cmd = []

  initialState :: state

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

  -- Most often the result of executing a command against the system under test
  -- will live in the IO monad, but sometimes it can be useful to be able a
  -- choose another monad.
  type CommandMonad state :: Type -> Type
  type CommandMonad state = IO
```

The interface is parametrised by a `state` type that the user needs to
define before instantiating the interface. In the counter example
`state` is `newtype Counter = Counter Int`. The user needs to provide an
`initialState :: state` from which we'll start generating commands and
executing the model, in the counter case this is `Counter 0`.

As part of the instantiating the user also needs to specify a type of
`Command`s and `Response`s, these were the `Incr` and `Get` operations
of the counter and their response types respectively.

In addition there's also three optional types, that we've not needed in
the counter example. The first is references, these are used to refer
back to previously created resources. For example if we open a file
handle on a POSIX-like filesystem, then later commands need to be able
to refer to that file handle when wanting to write or read from it. The
second datatype is `PreconditionFailure`, which is used to give a nice
error message when a command is executed in a disallowed state. For
example if we try to read from a file handle that has been closed. The
third data type is `CommandMonad` which let's us use a different monad
than `IO` for executing our commands in. After we've finished with the
interface definition we'll come back to more examples where we'll use
these optional types, hopefully these examples will help make things
more concrete.

We've already seen that the user needs to provide a way to generate
single commands, the only thing worth mentioning is that in case our
commands are parametrised by references then during the generation phase
we only deal with `Var`s of references, where `data Var a = Var Int`.
The reason for this is that we cannot generate, for example, real file
handles (only the operating system can), so instead we generate symbolic
references which are just `Int`s. Think of these as placeholders for
which real references will be substituted in once the real references
are created during execution.

Shrinking of individual commands is optional and disabled by default,
but as we've seen this doesn't exclude the sequence of commands to be
shrunk. We'll shall see shortly how that is done in detail.

Next up we got `runFake` and `runReal` which executes a command against
the `state` model and the real system respectively. Notice how `runFake`
can fail with a `PreconditionFailure`, whereas `runReal` is always
expected to succeed (because if a command fails the precondition check,
then it won't get generated and hence never reach `runReal`). Another
difference is that `runFake` uses symbolic references, while `runReal`
deals with real references. We'll shortly see how this substitution of
references works.

Lastly we have two optional functions related to keeping statistics of
generated test cases, which is useful for coverage reporting among other
things. We'll come back to how these can be used as we look at more
examples after we've defined our stateful property-based testing
library.

##### Generating and shrinking

Once we have our interface we can start writing functions against the
interface. These functions are what the user gets once they implement
the interface. In this section we'll have a look at generation of
sequences of `Commands`, which will be the inputs for our tests, and how
to shrink said inputs to produce a minimal counterexample.

Let's start by defining `Commands`, notice that they use symbolic
references (i.e. `Var (Reference state)`):

``` haskell
newtype Commands state = Commands
  { unCommands :: [Command state (Var (Reference state))] }
```

As mentioned above, when we generate commands we cannot generate real
references, e.g. file handles, thus `Var (Reference state)` is used
which is isomorphic to just an `Int`.

Sometimes it's convenient to split up `runFake` into two parts, the
first checks if the command is allowed in the current state, i.e. the
precondition holds:

``` haskell
precondition :: StateModel state
             => state -> Command state (Var (Reference state)) -> Bool
precondition s cmd = case runFake cmd s of
  Left _  -> False
  Right _ -> True
```

And the second part advances the state:

``` haskell
nextState :: StateModel state
          => state -> Command state (Var (Reference state)) -> state
nextState s cmd = case runFake cmd s of
  Right (s', _) -> s'
  Left _err -> error "nextState: impossible, we checked for success in precondition"
```

We assume that we'll only ever look at the `nextState` when the
`precondition` holds.

Using these two functions we can implement QuickCheck's `Arbitrary` type
class for `Commands` which let's us generate and shrink `Commands`:

``` haskell
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
```

``` haskell
  shrink :: Commands state -> [Commands state]
  shrink = pruneShrinks . possibleShrinks
    where
      possibleShrinks :: Commands state -> [Commands state]
      possibleShrinks = map (Commands . map fst) . shrinkList shrinker
                      . withStates initialState . unCommands
        where
          shrinker (cmd, s) = [ (cmd', s) | cmd' <- shrinkCommand s cmd ]

          withStates :: StateModel state
                     => state -> [Command state (Var (Reference state))]
                     -> [(Command state (Var (Reference state)), state)]
          withStates s0 = go s0 []
            where
              go _s acc []           = reverse acc
              go  s acc (cmd : cmds) = go (nextState s cmd) ((cmd, s) : acc) cmds

      pruneShrinks :: [Commands state] -> [Commands state]
      pruneShrinks = coerce . filter (not . null)
                   . map (go initialState Set.empty [] . unCommands)
        where
          go _s _vars acc [] = reverse acc
          go  s  vars acc (cmd : cmds)
            | not (scopeCheck vars cmd) = go s vars acc cmds
            | otherwise = case runFake cmd s of
                Left _preconditionFailure -> go s vars acc cmds
                Right (s', resp) ->
                  let
                    returnedVars = Set.fromList (toList resp)
                    vars' = returnedVars `Set.union` vars
                  in
                    go s' vars' (cmd : acc) cmds
```

Notice how after shrinking we prune away all commands that don't pass
the precondition.

The intuition here is that as we remove commands from the originally
generated `Commands` (which all pass their preconditions), we might have
broken some preconditions and pruning simply removes the commands which
we made invalid in the process of shrinking.

##### Running and assertion checking

Once we've generated `Commands` we need to execute them against the
model and the real system using `runFake` and `runReal`. In the process
of doing so `runReal` will produce `Reference`s that later commands
might use, so we also need to substitute symbolic references for real
references. This, together coverage statatistics bookkeeping, is done in
the `runCommands` function:

``` haskell
runCommands :: forall state. StateModel state
            => Commands state -> PropertyM (CommandMonad state) ()
runCommands (Commands cmds0) = go initialState emptyEnv cmds0
  where
    go :: state -> Env state -> [Command state (Var (Reference state))]
       -> PropertyM (CommandMonad state) ()
    go _state _env [] = return ()
    go  state  env (cmd : cmds) = do
      case runFake cmd state of
        Left err -> do
          monitor (counterexample ("Preconditon failed: " ++ show err))
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
              env'   = extendEnv env (zip [sizeEnv env..] refs)
              cresp' = fmap (lookupEnv env') resp
              ok     = cresp == cresp'
          unless ok $
            monitor (counterexample ("Expected: " ++ show cresp' ++ "\nGot: " ++ show cresp))
          -- And finally here's where we assert that the model and the real
          -- implementation agree.
          assert ok
          go state' env' cmds
```

Where `Env` is defined as follows.

``` haskell
newtype Env state = Env { unEnv :: IntMap (Reference state) }
```

That's all the pieces we need to implement that `Counter` example that
we saw above, plus some new constructs to deal with precondition
failures and references.

Next let's have a look at an example where we need preconditions and
references.

#### Example: circular buffer

This example is taken from John's paper [*Testing the hard stuff and
staying
sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
(2014).

##### Software under test

The implementation is written in C and uses two indices which keep track
of the front and back of the queue, this allows us to implement the
queue in a circular fashion. I've copied the C code straight from the
paper. In order to test it from Haskell, we'll use Haskell's foreign
function interface.

``` c
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

Notice that the C code doesn't do any error checking, e.g. if we `get`
from an empty queue then we'll get back uninitialised memory.

##### Model

The circular buffer implementation is very efficient, because it reuses
the allocated memory as we go around in circles, but it's not obviously
correct.

To model queues we'll use a more straight forward non-circular
implementation. This is less efficient (doesn't matter as it's merely
used during testing), but hopefully more obviously correct.

``` haskell
data FQueue = FQueue
  { fqElems :: [Int]
  , fqSize  :: Int
  }
```

In the `Counter` example above we only had one counter, so the model was
merely a single integer. In this example, because of `new` returning a
queue, we need to be able to model arbitrary many queues. We can do this
using symbolic references (`data Var a = Var Int`) as follows:

``` haskell
type State = Map (Var Queue) FQueue

emptyState :: State
emptyState = Map.empty
```

Where `Queue` is the Haskell data type that corresponds to the C `Queue`
and the `Var a` data type is provided by the library and is a symbolic
reference to `a` (just an `Int`eger). The idea being that in the model
we don't have access to real `Queue`s, merely symbolic references to
them. This might seem a bit strange, but I hope that it will become more
clear when we model `new`.

``` haskell
type FakeOp a = State -> Either Err (State, a)

fNew :: Int -> FakeOp (Var Queue)
fNew sz s =
  let
    v  = Var (Map.size s)
    s' = Map.insert v (FQueue [] sz) s
  in
    return (s', v)
```

As we have access to the state when defining our model, we can create
new unique symbolic references by simply counting how many symbolic
references we've created previously (using `Map.size`)[^2].

As we said before, in the C code we don't do any error checking. In the
model we do check that, for example, the queue is non-empty before we
`fGet` an item. These are our preconditions.

``` haskell
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

Recall that we won't generate a get operation unless the precondition
holds in the state that we are currently in, i.e. we will never generate
gets if the queue is empty and thus we'll never execute the C code for
`get` which gives back uninitialised memory.

Having defined our model the interface implementation is almost
mechanical.

``` haskell
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
`Response`, which is parametrised so that it works for both symbolic and
real references. The `Functor` instance let's us to substitution, while
`Foldable` let's us extract all new references from a response, so that
we can substitute them in later `Command`s.

##### Testing

Having implemented the interface, we can write our property as follows.

``` haskell
prop_queue :: Commands State -> Property
prop_queue cmds = monadicIO $ do
  runCommands cmds
  assert True
```

When we run it, using `quickCheck prop_queue`, we get the following
error.

       *** Failed! Assertion failed (after 7 tests and 5 shrinks):
        Commands {unCommands = [New 1,Put (Var 0) 0,Put (Var 0) 1,Get (Var 0)]}
        New 1 --> New_ (Queue 0x00000000016e9010)
        Put (Var 0) 0 --> Put_ ()
        Put (Var 0) 1 --> Put_ ()
        Get (Var 0) --> Get_ 1
        Expected: Get_ 0
        Got: Get_ 1

So we create a new queue of size `1`, put two items (`0` and `1`) into
it, and finally we read a value from the queue and this is where the
assertion fails. Or model returns `0`, because it's a FIFO queue, but
the C code returns `1`. The reason for this is that in the C code
there's no error checking, so writing a value to a full queue simply
overwrites the oldest value. So there's actually nothing wrong with the
implementation, but rather the model is wrong. We've forgotten a
precondition:

``` diff
 data Err
   = QueueDoesNotExist
   | QueueIsEmpty
+  | QueueIsFull
```

``` diff
fPut :: Var Queue -> Int -> State -> Either Err (State, ())
fPut q i s
  | q `Map.notMember` s = Left QueueDoesNotExist
+ | length (fqElems (s Map.! q)) >= fqSize (s Map.! q) = Left QueueIsFull
  | otherwise = return (Map.adjust (\fq -> fq { fqElems = fqElems fq ++ [i] }) q s, ())
```

We can add the counterexample that we got as a regression test to our
testsuite as follows:

``` haskell
unit_queueFull :: IO ()
unit_queueFull = quickCheck (withMaxSuccess 1 (expectFailure (prop_queue cmds)))
  where
    cmds = Commands
      [ New 1
      , Put (Var 0) 1
      , Put (Var 0) 0
      , Get (Var 0)
      ]
```

Notice that we can basically copy-paste `cmds` from QuickCheck's output,
but I've done some formatting here to make it more readable.

After fixing the precondition for `fPut`, `unit_queueFull` fails as
follows:

    +++ OK, failed as expected. Assertion failed (after 1 test):
    New 1 --> New_ (Queue 0x00000000006f6d20)
    Put (Var 0) 1 --> Put_ ()
    Preconditon failed: QueueIsFull

When we rerun `quickCheck prop_queue` we will not generate this example
again, because all preconditions need to hold, and the property passes:

    >>> quickCheck prop_queue
    +++ OK, passed 100 tests:
    95% New
    86% Put
    67% Get

    Commands (2497 in total):
    44.13% New
    41.25% Put
    14.62% Get

However as we can see in the output there's no coverage for `Size`! The
reason for this is because we've forgot to add it to our generator:

``` diff
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

    >>> quickCheck prop_queue
    *** Failed! Assertion failed (after 25 tests and 8 shrinks):
    Commands {unCommands = [New 1,Put (Var 0) 0,Size (Var 0)]}
    New 1 --> New_ (Queue 0x0000000001444220)
    Put (Var 0) 0 --> Put_ ()
    Size (Var 0) --> Size_ 0
    Expected: Size_ 1
    Got: Size_ 0

Size should return how many items are in the queue, so after we put one
item into a queue we expect it to return `1`, but in the above
counterexample it returns `0`.

To understand why this happens we have to look at how `put` and `size`
are implemented:

``` c
void put(Queue *q, int n) {
  q->buf[q->inp] = n;
  q->inp = (q->inp + 1) % q->size;
}

int size(Queue *q) {
  return (q->inp - q->outp) % q->size;
}
```

In `put` when we do `q->inp = (q->inp + 1) % q->size` we get
`q->inp = (0 + 1) % 1 == 0` and then when we calculate the `size` we get
`(0 - 0) % 1 == 0`. One way to fix this is to make `q->size` be `n + 1`
rather than `n` where `n` is the size parameter of `new`, that way `put`
will do `q->inp = (0 + 1) % 2 == 1` instead and size will be
`1 - 0 % 2 == 1` which is correct. Here's the diff:

``` diff
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

``` haskell
unit_queueSize :: IO ()
unit_queueSize = quickCheck (withMaxSuccess 1 (prop_queue cmds))
  where
    cmds = Commands
      [ New 1
      , Put (Var 0) 0
      , Size (Var 0)
      ]
```

After the change to `new` this test passes, but if we rerun the property
we get the following error:

    *** Failed! Assertion failed (after 38 tests and 12 shrinks):
    Commands {unCommands = [New 1,Put (Var 0) 0,Get (Var 0),Put (Var 0) 0,Size (Var 0)]}
    New 1 --> New_ (Queue 0x00007fd47c00a920)
    Put (Var 0) 0 --> Put_ ()
    Get (Var 0) --> Get_ 0
    Put (Var 0) 0 --> Put_ ()
    Size (Var 0) --> Size_ (-1)
    Expected: Size_ 1
    Got: Size_ (-1)

After the second `put` we'll have `q->inp = (1 + 1) % 2 == 0` while
`q->outp = 1` due to the `get` and so when we call `size` we get
`0 - 1 % 2 == -1`. Taking the absolute value:

``` diff
  int size(Queue *q) {
-   return (q->inp - q->outp) % q->size;
+   return abs(q->inp - q->outp) % q->size;
  }
```

Makes this test case pass, and in fact it also makes the property pass:

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

John says that at this point most programmers would probably be happy
and believe that their implementation works, but if we rerun it again
(or increase the amount of tests generated), we get:

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

We can see that all queues of size `1` now work, because this test
starts by creating a queue of size `2`, so we've made progress. But
taking the absolute value isn't the correct way to calculate the size
(even though it works for queues of size `1`), the following is the
correct way to do it:

``` diff
  int size(Queue *q) {
-   return abs(q->inp - q->outp) % q->size;
+   return (q->inp - q->outp + q->size) % q->size;
  }
```

With this final tweak, the property passes. I hope that this somewhat
long example gives you a feel for how property-based testing drives the
development and debugging of code.

#### Example: jug puzzle from Die Hard 3

In the movie Die Hard 3 there's an
[scene](https://www.youtube.com/watch?v=BVtQNK_ZUJg) where Bruce Willis
and Samuel L. Jackson have to solve a puzzle in order to stop a bomb
from going off. The puzzle is: given a 3L and a 5L jug, how can you
measure exactly 4L?

I first saw this example solved using TLA+ and I wanted to include it
here because it shows that we don't necessarily need a real
implementation, merely running the model/fake can be useful.

The main idea is to model the two jugs and all actions we can do with
them and then throw an exception when the big jug contains 4L. This will
fail the test and output the shrunk sequence of actions that resulted in
the failure, giving us the solution to the puzzle.

``` haskell
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

Notice how the trace shows the intermediate states, making it easy to
verify that it's indeed a correct solution to the puzzle[^3].

### Parallel property-based testing in ~230 LOC

Let's now turn our focus to parallel property-based testing.

Debugging buggy concurrent code is not fun. The main reason for this is
that the threads interleave in different ways between executions, making
it hard to reproduce the bug and hard to verify that a bug fix actually
worked.

Ideally we'd like to make working with concurrent code as pleasant as
the sequential stateful case and without the user having to write any
additional test code.

In order to explain how we can achieve this, we need to first understand
how we can test concurrent code in a reproducible way.

Recall our `Counter` that we looked at in the sequential testing case.
Here we'll be using a slight generalisation where the `incr` takes an
integer parameter which specifies by how much we want to increment (as
opposed to always incrementing by `1`).

``` haskell
>>> incr 1
>>> incr 2
>>> get
3
```

When we interact with the counter sequentially, i.e. one command at the
time, then it appears to count correctly.

But if we instead concurrently issue the `incr`ements , we see something
strange:

``` haskell
 > forM_ [0..100000] $ \i -> do
 >   c <- newCounter
 >   concurrently_ (incr c 1) (incr c 2)
 >   x <- get c
 >   if x == 3 then return () else error ("i = " ++ show i ++ ", x = " ++ show x)
 *** Exception: i = 29768, x = 1
```

After 29768 iterations we get back `1` rather than the expected `3`! The
reason for this is because there's a race condition in the
implementation of `incr`:

``` haskell
 incr i = do
   j <- readIORef gLOBAL_COUNTER
   writeIORef gLOBAL_COUNTER (i + j)
```

Because we first read the old value and *then* write the new incremented
value in an non-atomic way, it's possilbe that if two threads do this at
the same time they overwrite each others increment. For example:

       thread 1, incr 1     |  thread 2, incr 2
       ---------------------+------------------
        0 <- readIORef      |
                            | 0 <- readIORef
                            | writeIORef (2 + 0)
        writeIORef (1 + 0)  |
                            |
                            v
                           time

If we read from the counter after the two increments are done we get `1`
instead of the expected `3`. The fix to this problem is to do an atomic
update using `atomicModifyIORef'`, instead of first reading and then
writing to the `IORef`.

The concurrent test that we just wrote is not only specific to the
counter example but also only uses three fixed commands, the two
concurrent `incr`ements followed by a `get`. While it was enough to find
this race condition, in general we'd like to try arbitrary combinations
of commands and possibly involving more than two threads.

The key concept we need in order to accomplish that is that of
*concurrent history*, which is perhaps easiest to explain in terms of a
more familiar concept: a sequence diagram.

Consider the following sequence diagram:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/sequence-diagram.svg"
width=60%>

Here we see that the first and second thread concurrently increment, the
first thread then reads the counter concurrently with the second
thread's increment that's still going on. The second thread's increment
finishes and a third thread does a read which is concurrent with the
first thread's read.

We can abstract away the arrows and merely focus on the intervals of the
commands:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/history-from-sequence-diagram.svg"
width=60%>

If we rotate the intervals we get the concurrent history:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/concurrent_counter.svg"
width=60%>

Note that the execution of some commands overlap in time, this is what's
meant by concurrent and arguebly it's easier to see the overlap here
than in the original sequence diagram.

We've also abstracted away the counter, it's a black box from the
perspective of the threads. The only thing we know for sure is when we
invoked the operation and when it returned, which is what our interval
captures. We also know that the effect of the operation must have
happend sometime within that interval.

One such concurrent history can have different interleavings, depending
on when exactly the effect of the commands happen. Here are two possible
interleavings, where the red cross symbolises when the effect happened
(i.e. when exactly the counter update its state).

The first corresponds to the sequential history
`< incr 1, get, incr 2, get >`:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/concurrent_counter_get_1_3.svg"
width=60%>

and the other interleaving corresponds to the sequential history
`< incr 1, incr 2, get, get >`:

<img
src="https://raw.githubusercontent.com/stevana/stateful-pbt-with-fakes/main/images/concurrent_counter_get_3_3.svg"
width=60%>

One last thing we've left out from the concurrent history so far is the
responses. In this example, the only interesting responses are those of
the `get`s.

Let's say that the `get`s returned `1` and `3` respectively. Is this a
correct concurrent outcome? Yes, according to linearisability it's
enough to find a single interleaving for which the sequential state
machine model can explain the outcome and in this case the first
interleaving above `< incr 1, get, incr 2, get >` does that.

What if the `get`s both returned `3`? That's also correct and witnessed
by the second interleaving `< incr 1, incr 2, get, get >`. When we can
find a sequential interleaving that supports the outcome of a concurrent
execution we say that the concurrent history linearises.

If the `get` on the third thread returned `1` or `2` however, then it
would be a non-linearisable outcome. We can see visually that that `get`
happens after both `incr`, so no matter where we choose to place the red
crosses on the `incr`s the effects will happen before that `get` so it
must return `3`. Is it even possilbe that `1` or `2` are returned? It's,
imagine if `incr` is implemented by first reading the current value then
storing the incremented value, in that case there can be a race where
the `incr`s overwrite each other.

So to summarise, we execute commands concurrently using several threads
and gather a concurrent history of the execution. We then try to find a
sequential interleaving (a choice of where the red crosses in the
diagrams should be) which respects the a sequential state machine model
specfication. If we find a single one that does, then we say that the
history linearises and that the concurrent execution is correct, if we
cannot find a sequential interleaving that respects the model then the
history doesn't linearise and we have found a problem.

#### Parallel library implementation

Let's try to implement the above. We'll split up the implementation in
three parts. First we'll show how to generate and shrink parallel
commands, these will be different than the sequential commands as we
have more than one thread that does the execution. Second we'll
implement linearisability checking by trying to find an interleaving of
the concurrent history which respects the sequential model. Finally,
we'll have a look at how to execute the generated parallel commands to
produce a concurrent history.

##### Parallel program generation and shrinking

First we need define what a parallel program is:

``` haskell
newtype ParallelCommands state = ParallelCommands [Fork state]

newtype Fork state = Fork [Command state (Var (Reference state))]
```

The idea is that the commands inside `Fork`s get executed in parallel,
this list will only be between one and three commands long, i.e.
capturing single, double or triple threaded exeuction. The amount of
`Fork`s themselves vary with the size of the test case, just like when
we were doing the sequential testing.

Depending on the order in which the commands in the `Fork`s get
executed, we can potententially get different models. For example
`Fork [Write "a" "foo", Write "a" "bar"]`, depending on which branch of
the `Fork` gets executed first we might end up with either `"foo"` or
`"bar"` being written to `"a"`.

Because of this, we have generalised generation and shrinking to work on
a set of states rather than just a single state:

``` haskell
class (StateModel state, Ord state) => ParallelModel state where

  generateCommandParallel :: [state] -> Gen (Command state (Var (Reference state)))
  generateCommandParallel ss = do
    s <- elements ss
    generateCommand s

  shrinkCommandParallel :: [state] -> Command state (Var (Reference state))
                        -> [Command state (Var (Reference state))]
  shrinkCommandParallel ss cmd = shrinkCommand (maximum ss) cmd
```

Notice that the default implementation for generation, which should be
good enough for most examples, picks an arbitrary state and reuses the
generation function from the sequential case. Similar shrinking picks
the biggest state (determined by the `Ord` instance) as the default
implementation. The user is able to override these defaults, in case
generation or shrinking depends on some more specific state.

We can now write a generator for parallel programs.

``` haskell
  arbitrary :: Gen (ParallelCommands state)
  arbitrary = ParallelCommands <$> go [initialState]
    where
      go :: [state] -> Gen [Fork state]
      go ss = sized $ \n ->
        let
          w = n `div` 2 + 1
        in
          frequency
            [ (1, return [])
            , (w, do k <- frequency [ (50, return 1) -- 50% single threaded
                                    , (30, return 2) -- 30% double threaded
                                    , (20, return 3) -- 20% triple threaded
                                    ]
                     mCmds <- vectorOf k (generateCommandParallel ss)
                                `suchThatMaybe` (parallelSafe ss . Fork)
                     case mCmds of
                       Nothing   -> return []
                       Just cmds ->
                         (Fork cmds :) <$> go (nextStates ss cmds))
            ]
```

Where `nextStates` gives all potential next states and is defined as
follows.

``` haskell
nextStates :: (StateModel state, Ord state)
           => [state] -> [Command state (Var (Reference state))] -> [state]
nextStates ss cmds = nubOrd [ foldl' nextState s cmds | s <- ss ]
```

The other helper function that we need for generation is `parallelSafe`,
which requires a bit of background.

In the sequential case a precondition is a contract that needs to be
fulfilled by the client before the command is issued. In the parallel
case there are multiple clients, so it could be the case that one client
unknowingly breaks another clients precondition.

E.g. `Fork [Write "a" "foo", Delete "a"]`, where the precondition for
both commands is that `"a"` exists. If `Delete` gets executed first then
it would break `Write`'s precondition.

One idea might be to drop all preconditions in the parallel case and
make all commands be able to fail gracefully instead of crashing, e.g.
`Write_ (Either DoesntExist ())`.

XXX: make counter or queue example into one of such examples:

The problem with this approach is that examples such as the ticket
dispenser have initialisation commands such as `New` which create a
ticket dispenser reference upon which the later commands depend on, so
without preconditions forbidding more than one `New` we can end up
generating: `Fork New New`, which doesn't make sense. It should also be
noted that making `New` fail gracefully when a `New` has already been
executed would need a global boolean flag, which is ugly.

The solution to the preconditon problem is to check that they hold in
all possible interleavings of a `Fork`, which is what `parallelSafe`
does:

``` haskell
parallelSafe :: ParallelModel state => [state] -> Fork state -> Bool
parallelSafe ss (Fork cmds0) = and
  [ preconditionsHold s cmds | s <- toList ss, cmds <- permutations cmds0 ]
  where
    preconditionsHold s0 = all (go s0) . permutations
      where
        go _s [] = True
        go  s (cmd : cmds)
          | precondition s cmd = go (nextState s cmd) cmds
          | otherwise          = False
```

While shrinking we also use `parallelSafe`:

``` haskell
  shrink :: ParallelCommands state -> [ParallelCommands state]
  shrink = pruneShrinks . possibleShrinks
    where
      possibleShrinks :: ParallelCommands state -> [ParallelCommands state]
      possibleShrinks
        = map (coerce . map (map fst))
        . shrinkList (shrinkList shrinker) . withParStates . unParallelCommands
        where
          withParStates :: (StateModel state, Ord state)
                        => [Fork state]
                        -> [[(Command state (Var (Reference state)), [state])]]
          withParStates = go [initialState] [] . coerce
            where
              go _ss acc []             = reverse acc
              go  ss acc (cmds : cmdss) =
                go (nextStates ss cmds) (map (\cmd -> (cmd, ss)) cmds : acc) cmdss

          shrinker :: (Command state (Var (Reference state)), [state])
                   -> [(Command state (Var (Reference state)), [state])]
          shrinker (cmd, ss) = [ (cmd', ss) | cmd' <- shrinkCommandParallel ss cmd ]

      pruneShrinks :: [ParallelCommands state] -> [ParallelCommands state]
      pruneShrinks = coerce . filter (not . null)
                   . map (go [initialState] Set.empty [] . unParallelCommands)
        where
          go :: [state] -> Set (Var (Reference state)) -> [Fork state] -> [Fork state] -> [Fork state]
          go _ss _vars acc [] = reverse acc
          go  ss  vars acc (fork@(Fork cmds) : forks)
            | all (scopeCheck vars) cmds
            , parallelSafe ss fork =
              let
                ss'   = nextStates ss cmds
                vars' = getReturnedVars (head ss) vars cmds -- NOTE: head is safe
              in
                go ss' vars' (fork : acc) forks
            | otherwise            = go ss vars acc forks

          -- It doesn't matter which of the possible states we start in, as all
          -- commands in a fork pass their preconditions in all states. It also
          -- doesn't matter in which interleaving we gather the responses, as
          -- all we do is collect the `Var`s that get returned into an unordered
          -- `Set`.
          getReturnedVars _s vars [] = vars
          getReturnedVars s vars (cmd : cmds) = case runFake cmd s of
            Left _preconditionFailed ->
              error "getReturnedVars: impossible, parallelSafe checks that all preconditions hold"
            Right (_s', resp) ->
              getReturnedVars s (vars `Set.union` Set.fromList (toList resp)) cmds
```

In addition we also check that shrinking doesn't create any scoping
issues, i.e. if we remove a command which creates a symbolic variable we
also need to remove any fork that contains a command which uses said
symbolic variable.

Another option is to skip the scope checking and instead require the
user to explicitly require preconditions which ensure the scope.

We can also improve the shrinking by moving commands outside of forks,
e.g. `[Fork [a, b]] ==> [Fork [a], Fork [b]]`, thus making the program
more sequential, and moving forks after smaller forks, e.g.
`[Fork [a, b], Fork [c]] ==> [Fork [c], Fork [a, b]]`, thus making for
less potential concurrent interleavings. For simplicity, we've chosen
not to implemented those here.

##### Parallel running

One final difference in the parallel case is that because of the use of
threads to achieve parallel execution, and the fact we can only spawn
threads of things of type `IO`, we also need to be able to interpret our
`CommandMonad` into `IO`, which is what `runCommandMonad` does.

``` haskell
  -- If another command monad is used we need to provide a way run it inside the
  -- IO monad. This is only needed for parallel testing, because IO is the only
  -- monad we can execute on different threads.
  runCommandMonad :: proxy state -> CommandMonad state a -> IO a
```

We can now implement parallel execution of commands as follows:

``` haskell
newtype History state = History [Event state]
deriving stock instance
   (Show (Command state (Var (Reference state))),
    Show (Response state (Reference state))) => Show (History state)

data Event state
  = Invoke Pid (Command state (Var (Reference state)))
  | Ok     Pid (Response state (Reference state))
deriving stock instance
  (Show (Command state (Var (Reference state))),
   Show (Response state (Reference state))) => Show (Event state)

newtype Pid = Pid Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Enum
```

``` haskell
runParallelCommands :: forall state. ParallelModel state
                    => ParallelCommands state -> PropertyM IO ()
runParallelCommands cmds0@(ParallelCommands forks0) = do
  forM_ (parallelCommands cmds0) $ \cmd -> do
    let name = commandName cmd
    monitor (tabulate "Commands" [name] . classify True name)
  monitor (tabulate "Concurrency" (map (show . length . unFork) forks0))
  q   <- liftIO newTQueueIO
  c   <- liftIO newAtomicCounter
  env <- liftIO (runForks q c emptyEnv forks0)
  hist <- History <$> liftIO (atomically (flushTQueue q))
  let ok = linearisable env (interleavings hist)
  unless ok (monitor (counterexample (show hist)))
  assert ok
  where
    runForks :: TQueue (Event state) -> AtomicCounter -> Env state -> [Fork state]
             -> IO (Env state)
    runForks _q _c env [] = return env
    runForks  q  c env (Fork cmds : forks) = do
      envs <- liftIO $
        mapConcurrently (runParallelReal q c env) (zip [Pid 0..] cmds)
      let env' = combineEnvs (env : envs)
      runForks q c env' forks

    runParallelReal :: TQueue (Event state) -> AtomicCounter -> Env state
                    -> (Pid, Command state (Var (Reference state))) -> IO (Env state)
    runParallelReal q c env (pid, cmd) = do
      atomically (writeTQueue q (Invoke pid cmd))
      eResp <- try (runCommandMonad (Proxy :: Proxy state) (runReal (fmap (lookupEnv env) cmd)))
      case eResp of
        Left (err :: SomeException) ->
          error ("runParallelReal: " ++ displayException err)
        Right resp -> do
          -- NOTE: It's important that we extend the environment before writing `Ok`
          -- to the history, otherwise we might get scope issues.

          -- XXX: Move outside of mapConcurrently? How do we assign the right `Var`
          -- with each `Reference`? Perhaps this would be easier if we had a prefix
          -- and N suffixes?
          env' <- extendEnvParallel env c (toList resp)
          atomically (writeTQueue q (Ok pid resp))
          return env'
```

Extending the environment in the parallel case requires an atomic
counter in order to avoid more than one thread adding the same variable:

``` haskell
newtype AtomicCounter = AtomicCounter (IORef Int)

newAtomicCounter :: IO AtomicCounter
newAtomicCounter = AtomicCounter <$> newIORef 0

-- Returns old value.
incrAtomicCounter :: AtomicCounter -> Int -> IO Int
incrAtomicCounter (AtomicCounter ioRef) n =
  atomicModifyIORef' ioRef (\old -> (old + n, old))

extendEnvParallel :: Env state -> AtomicCounter -> [Reference state] -> IO (Env state)
extendEnvParallel env c refs = do
  i <- incrAtomicCounter c (length refs)
  return (extendEnv env (zip [i..] refs))

combineEnvs :: [Env state] -> Env state
combineEnvs = Env . IntMap.unions . map unEnv
```

Hopefully the execution part is clear, next let's have a look at how we
check the result of an execution.

##### Linearisability checking

Recall from our parallel counter example in the introduction to parallel
testing that it's enough to find *any* possible interleaving which
respects the sequential model. So let's start by enumerating all
possible interleavings using a [`Rose`
tree](https://hackage.haskell.org/package/containers-0.7/docs/Data-Tree.html)
datastrucutre:

``` haskell
data Op state = Op (Command state (Var (Reference state)))
                   (Response state (Reference state))

interleavings :: History state -> Forest (Op state)
interleavings (History [])  = []
interleavings (History evs0) =
  [ Node (Op cmd resp) (interleavings (History evs'))
  | (tid, cmd)   <- takeInvocations evs0
  , (resp, evs') <- findResponse tid
                      (filter1 (not . matchInvocation tid) evs0)
  ]
  where
    takeInvocations :: [Event state] -> [(Pid, Command state (Var (Reference state)))]
    takeInvocations []                         = []
    takeInvocations ((Invoke pid cmd)   : evs) = (pid, cmd) : takeInvocations evs
    takeInvocations ((Ok    _pid _resp) : _)   = []

    findResponse :: Pid -> [Event state] -> [(Response state (Reference state), [Event state])]
    findResponse _pid []                                   = []
    findResponse  pid ((Ok pid' resp) : evs) | pid == pid' = [(resp, evs)]
    findResponse  pid (ev             : evs)               =
      [ (resp, ev : evs') | (resp, evs') <- findResponse pid evs ]

    matchInvocation :: Pid -> Event state -> Bool
    matchInvocation pid (Invoke pid' _cmd) = pid == pid'
    matchInvocation _   _                  = False

    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 _ []                   = []
    filter1 p (x : xs) | p x       = x : filter1 p xs
                       | otherwise = xs
```

We can then check if there is a path through this rose tree which agrees
with the sequential model:

``` haskell
linearisable :: forall state. StateModel state
             => Env state -> Forest (Op state) -> Bool
linearisable env = any' (go initialState)
  where
    go :: state -> Tree (Op state) -> Bool
    go s (Node (Op cmd cresp) ts) =
      case runFake cmd s of
        Left _preconditionFailure ->
          error "linearisable: impossible, all precondtions are satisifed during generation"
        Right (s', resp) ->
          cresp == fmap (lookupEnv env) resp && any' (go s') ts

    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs
```

#### Example: parallel counter

This is the only new code we need to add to enable parallel testing of
our `Counter` example[^4] from before:

``` haskell
instance ParallelModel Counter where

  -- The command monad is IO, so we don't need to do anything here.
  runCommandMonad _ = id

prop_parallelCounter :: ParallelCommands Counter -> Property
prop_parallelCounter cmds = monadicIO $ do
  replicateM_ 10 $ do
    run reset
    runParallelCommands cmds
  assert True
```

If we run the above property with `runReal`

``` haskell
  runReal Incr = Incr_ <$> incrRaceCondition
```

being implemented using an increment with a race condition:

``` haskell
incrRaceCondition :: IO ()
incrRaceCondition = do
  n <- readIORef gLOBAL_COUNTER
  writeIORef gLOBAL_COUNTER (n + 1)
```

then a failure is found:

     Assertion failed (after 36 tests and 1 shrink):
          ParallelCommands [Fork [Incr,Incr],Fork [Incr],Fork [Get,Get,Get],Fork [Incr],Fork [Get,Get],Fork [Get,Get],Fork [Incr],Fork [Get,Get,Incr],Fork [Incr],Fork [Get,Get,Get],Fork [Incr,Incr],Fork [Incr,Get],Fork [Incr,Incr],Fork [Get],Fork [Incr]]

But shrinking didn't work very well. The reason for this is that
QuickCheck tries a smaller test case (which still has the race
condition), but because of a different interleaving of threads the race
doesn't get triggered and so QuickCheck thinks it found the minimal test
case (because the smaller test case, that the shriker picked, passes).

The proper solution to this problem is to use a deterministic thread
scheduler, this is what they do the parallel testing paper. A simpler
workaround is to introduce a small sleep after each read or write to
shared memory, this will make it more likely that the same interleaving
happens when we shrink the test:

``` haskell
incrRaceCondition :: IO ()
incrRaceCondition = do
  n <- readIORef gLOBAL_COUNTER
  threadDelay 100
  writeIORef gLOBAL_COUNTER (n + 1)
  threadDelay 100
```

With this change we get the minimal test case that triggers the race
condition:

    Assertion failed (after 6 tests and 4 shrinks):
          ParallelCommands [Fork [Incr,Incr],Fork [Get]]

We can avoid having to sprinkle sleeps around our interaction with
shared state by creating a module with the same operations as on shared
memory where the sleep is already included:

``` haskell
module SleepyIORef (module SleepyIORef, IORef) where

import Control.Concurrent (threadDelay)
import Data.IORef (IORef)
import qualified Data.IORef as IORef

------------------------------------------------------------------------

readIORef :: IORef a -> IO a
readIORef ref = do
  threadDelay 1000
  IORef.readIORef ref
```

That way if we find a race, we can change the import from
`import Data.IORef` to `import SleepyIORef` and rerun the tests and get
better shrinking.

This situation is not ideal, but save us the trouble of having to
reimplement a scheduler.

It's worth stressing that the race is found in the unmodified code and
the introduction of sleep is only needed to make the counterexample
smaller.

#### Example: process registry

For a slightly more complicated example containing race conditions,
let's have a look at an implementation of the Erlang process
registry[^5].

##### Software under test

The idea behind Erlang's process registry is that you can spawn threads,
register the `ThreadId` to some name of type string, and then lookup the
thread by name rather than its thread id. Threads can also be
unregistered and killed.

This is useful if threads die and get restarted and register the same
name, then other threads can easily find the thread id of the new thread
using the registry.

``` haskell
alive :: ThreadId -> IO Bool
alive tid = do
  s <- threadStatus tid
  return $ s /= ThreadFinished && s /= ThreadDied

{-# NOINLINE registry #-}
registry :: IORef [(String,ThreadId)]
registry = unsafePerformIO (newIORef [])

spawn :: IO ThreadId
spawn = forkIO (threadDelay 100000000)

whereis :: String -> IO (Maybe ThreadId)
whereis name = do
  reg <- readRegistry
  return $ lookup name reg

register :: String -> ThreadId -> IO ()
register name tid = do
  ok <- alive tid
  reg <- readRegistry
  if ok && name `notElem` map fst reg && tid `notElem` map snd reg
    then do
      atomicModifyIORef registry $ \reg' ->
           if name `notElem` map fst reg' && tid `notElem` map snd reg'
             then ((name,tid):reg',())
             else (reg',badarg)
    else badarg

unregister :: String -> IO ()
unregister name = do
  reg <- readRegistry
  if name `elem` map fst reg
    then atomicModifyIORef registry $ \reg' ->
           (filter ((/=name).fst) reg',
            ())
    else badarg

readRegistry :: IO [(String, ThreadId)]
readRegistry = do
  reg <- readIORef registry
  garbage <- filterM (fmap not.alive) (map snd reg)
  atomicModifyIORef' registry $ \reg' ->
    let reg'' = filter ((`notElem` garbage).snd) reg' in (reg'',reg'')

badarg :: a
badarg = error "bad argument"

kill :: ThreadId -> IO ()
kill tid = do
  killThread tid
  waitUntilDead 1000
  where
    waitUntilDead :: Int -> IO ()
    waitUntilDead 0 = error "kill: thread didn't die"
    waitUntilDead n = do
      b <- alive tid
      if b
      then do
        threadDelay 1000
        waitUntilDead (n - 1)
      else return ()
```

##### Model

``` haskell
data RegState = RegState
  { tids   :: [Var ThreadId]
  , regs   :: [(String, Var (ThreadId))]
  , killed :: [Var ThreadId]
  }
  deriving (Eq, Ord, Show)

instance StateModel RegState where

  initialState :: RegState
  initialState = RegState [] [] []

  type Reference RegState = ThreadId

  data Command RegState tid
    = Spawn
    | WhereIs String
    | Register String tid
    | Unregister String
    | Kill tid
    deriving (Show, Functor, Foldable)

  data Response RegState tid
    = Spawn_ tid
    | WhereIs_ (NonFoldable (Maybe tid))
    | Register_ (Either ErrorCall ())
    | Unregister_ (Either ErrorCall ())
    | Kill_ ()
    deriving (Eq, Show, Functor, Foldable)

  generateCommand :: RegState -> Gen (Command RegState (Var ThreadId))
  generateCommand s = oneof $
    [ return Spawn ] ++
    [ Register <$> arbitraryName <*> elements (tids s) | not (null (tids s)) ] ++
    [ Unregister <$> arbitraryName
    , WhereIs <$> arbitraryName
    ] ++
    [ Kill <$> elements (tids s) | not (null (tids s)) ]
    where
      arbitraryName :: Gen String
      arbitraryName = elements allNames

  runFake :: Command RegState (Var ThreadId)-> RegState
          -> Either void (RegState, Response RegState (Var ThreadId))
  runFake Spawn               s = let tid = Var (length (tids s)) in
                                  return (s { tids = tids s ++ [tid] }, Spawn_ tid)
  runFake (WhereIs name)      s = return (s, WhereIs_ (NonFoldable (lookup name (regs s))))
  runFake (Register name tid) s
    | tid `elem` tids s
    , name `notElem` map fst (regs s)
    , tid `notElem` map snd (regs s)
    , tid `notElem` killed s
    = return (s { regs = (name, tid) : regs s }, Register_ (Right ()))

    | otherwise
    = return (s, Register_ (Left (ErrorCall "bad argument")))

  runFake (Unregister name)   s
    | name `elem` map fst (regs s) =
        return (s { regs = remove name (regs s) }, Unregister_ (Right ()))
    | otherwise = return (s, Unregister_ (Left (ErrorCall "bad argument")))
    where
      remove x = filter ((/= x) . fst)
  runFake (Kill tid) s = return (s { killed = tid : killed s
                                   , regs   = remove tid (regs s)}, Kill_ ())
    where
      remove x = filter ((/= x) . snd)

  runReal :: Command RegState ThreadId -> IO (Response RegState ThreadId)
  runReal Spawn               = Spawn_      <$> spawn
  runReal (WhereIs name)      = WhereIs_ . NonFoldable <$> whereis name
  runReal (Register name tid) = Register_   <$> fmap (left abstractError) (try (register name tid))
  runReal (Unregister name)   = Unregister_ <$> fmap (left abstractError) (try (unregister name))
  runReal (Kill tid)          = Kill_       <$> kill tid

  monitoring :: (RegState, RegState) -> Command RegState ThreadId
             -> Response RegState ThreadId -> Property -> Property
  monitoring (_s, s') cmd resp =
    let
      aux tag = classify True (show tag)
              . counterexample ("\n    State: " ++ show s' ++ "\n")
    in
      case (cmd, resp) of
        (Register   {}, Register_   (Left _))  -> aux RegisterFailed
        (Register   {}, Register_   (Right _)) -> aux RegisterSucceeded
        (Unregister {}, Unregister_ (Left _))  -> aux UnregisterFailed
        (Unregister {}, Unregister_ (Right _)) -> aux UnregisterSucceeded
        _otherwise -> counterexample $ "\n    State: " ++ show s' ++ "\n"

-- Throws away the location information from the error, so that it matches up
-- with the fake.
abstractError :: ErrorCall -> ErrorCall
abstractError (ErrorCallWithLocation msg _loc) = ErrorCall msg

allNames :: [String]
allNames = ["a", "b", "c", "d", "e"]

data Tag = RegisterFailed | RegisterSucceeded | UnregisterFailed | UnregisterSucceeded
  deriving Show

prop_registry :: Commands RegState -> Property
prop_registry cmds = monadicIO $ do
  void (run cleanUp)
  runCommands cmds
  assert True

cleanUp :: IO [Either ErrorCall ()]
cleanUp = sequence
  [ try (unregister name) :: IO (Either ErrorCall ())
  | name <- allNames
  ]
```

One new thing to note here is that `WhereIs_` returns the thread id that
we wanted to look up, but thread ids also happen to be references. The
way we implemented extending the environment with new references is that
we call `Data.Foldable.toList` on all responses, which gives us all
references from the responses. In the `Spawn_` case this does the right
thing, since spawn returns a reference to the newly spawned thread id,
but in this case the thread id from `WhereIs_` is not a new reference
(it's merely a reference to the thread id we wanted to look up), so we
shouldn't extend the environment with the reference that `WhereIs_`
returns. We solve this problem with wrapping the response of `WhereIs_`
in `NonFoldable` which has a `toList` which doesn't return anything.

``` haskell
newtype NonFoldable a = NonFoldable a
  deriving stock (Eq, Show)

instance Functor NonFoldable where
  fmap f (NonFoldable x) = NonFoldable (f x)

instance Foldable NonFoldable where
  foldMap _f (NonFoldable _x) = mempty
```

###### Testing

The above passes the sequential tests and we can see that we got good
coverage of failing commands as well:

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

To make sure everything works as expected, let's introduce a bug on
purpose:

``` diff
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

If we rerun the tests with this bug in place, we get test failures like
the following:

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

As we can see unregister fails, when it in fact so should succeed (we've
registered `"e"` so we should be allowed to unregister it, but the real
implementation has due to the bug forgot that the registration
happened).

Let's move on to the parallel tests, all we need to add is:

``` haskell
instance ParallelModel RegState where
  runCommandMonad _ = id

prop_parallelRegistry :: ParallelCommands RegState -> Property
prop_parallelRegistry cmds = monadicIO $ do
  replicateM_ 10 $ do
    void (run cleanUp)
    runParallelCommands cmds
  assert True
-- start snippet ParallelRegistry
```

When we run the tests we get rather long counterexamples:

          *** Failed! (after 24 tests and 7 shrinks):
          Exception:
            bad argument
            CallStack (from HasCallStack):
              error, called at src/Example/Registry/Real.hs:69:10 in stateful-pbt-with-fakes-0.0.0-inplace:Example.Registry.Real
          ParallelCommands [Fork [Spawn,WhereIs "a"],Fork [Spawn],
                            Fork [Register "c" (Var 1),Spawn],Fork [Register "e" (Var 2),Register "a" (Var 2)]]

But if we replace our shared memory operations with version that do a
bit of sleep beforehand:

``` diff
- import Data.IORef
+ import SleepyIORef
```

We get better shrinking results:

          *** Failed! (after 5 tests and 5 shrinks):
          Exception:
            bad argument
            CallStack (from HasCallStack):
              error, called at src/Example/Registry/Real.hs:69:10 in stateful-pbt-with-fakes-0.0.0-inplace:Example.Registry.Real
          ParallelCommands [Fork [Spawn],Fork [Register "b" (Var 0),Register "c" (Var 0)]]

Here we see clearly that there's some problem in `Register`, as that's
the only thing that happens in parallel. If we look at the
implementation of `register` it's obvious where the race condition is,
for example we are using `atomicModifyIORef` to update the registry. The
problem is that we call `readRegistry` to check if a name has already
been registered and then call `atomicModifyIORef`, so the race can be if
another thread sneaks in between those two calls.

We can fix this problem by adding a global lock around `register`:

``` haskell
{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO (newMVar ())

registerNoRace :: String -> ThreadId -> IO ()
registerNoRace name tid = withMVar lock $ \_ -> register name tid
```

When rerunning the tests with this fixed version of `registry`, we get:

          *** Failed! Assertion failed (after 30 tests and 13 shrinks):
          ParallelCommands [Fork [Spawn],Fork [Spawn],Fork [Spawn],
                            Fork [Register "d" (Var 2)],Fork [Unregister "d",Unregister "d"]]

Which seems to suggest that we have a similar problem with `unregister`,
which is indeed the case. After applying the same fix to `unregister`,
we get:

          *** Failed! Assertion failed (after 15 tests and 4 shrinks):
          ParallelCommands [Fork [Spawn],Fork [Register "d" (Var 0)],
                            Fork [Kill (Var 0),Register "e" (Var 0)]]

Killing a thread will unregister it, so we get a similar problem again.
If we take the lock before calling `kill`, then the parallel tests
finally pass.

#### Example: key-value store

- change so it has a create command which creates a new ref?
- coverage to confirm

### Integration testing with contract tested fakes

Throughout this post we've used in-memory models, or fakes, as reference
implementations to test against.

The use of fakes diverges from the original work on Erlang QuickCheck,
where a more traditional state machine specification is used with
post-conditions.

As far as I know, Edsko de Vries'
[post](https://www.well-typed.com/blog/2019/01/qsm-in-depth/) (2019) was
the first to propose the use of fakes instead of state machine
specifications with post-conditions. Edsko also showed how one can
implement fake-based specifications on top of a library that uses state
machine specifications.

XXX: Post-conditions are more general than fakes? Relational vs
functional?

Fake instead of state machine spec is not only easier for programmers
unfamilar with formal specification

But there are other advantages to having a fake, for example we can use
this fake in integration tests with components that depend on the
software that we tested with the fake.

One of the problems with integration testing against fakes is that the
fake can be wrong. The standard solution to solve that problem is to
[contract test](https://martinfowler.com/bliki/ContractTest.html) the
fake to make sure that it is faithful to the software it's supposed to
be a fake of. We don't have this problem, because our tests assure that
the fake is faithful.

This, final, section is about unpacking and giving examples of how
integration testing against fakes works.

#### Example: queue (again)

As our first example of integration testing, let's recall our queue
example from the section on stateful testing. We can introduce an
interface for it as follows:

``` haskell
data IQueue q = IQueue
  { iNew  :: Int -> IO q
  , iPut  :: q -> Int -> IO ()
  , iGet  :: q -> IO Int
  , iSize :: q -> IO Int
  }
```

The real implementation can instantiate this interface in a
straightforward way:

``` haskell
real :: IQueue Queue
real = IQueue
  { iNew  = new
  , iPut  = put
  , iGet  = get
  , iSize = size
  }
```

The interesting part is that our fake can also instantiate the same
interface by storing the state in a mutable reference (`IORef`) as
follows.

``` haskell
fake :: IO (IQueue (Var Queue))
fake = do
  ref <- newIORef emptyState
  return IQueue
    { iNew  = \n   -> updateIORef ref (fNew n)
    , iPut  = \q i -> updateIORef ref (fPut q i)
    , iGet  = \q   -> updateIORef ref (fGet q)
    , iSize = \q   -> updateIORef ref (fSize q)
    }
  where
    updateIORef :: IORef State -> FakeOp a -> IO a
    updateIORef ref op =
      atomicModifyIORef' ref (\fs -> assoc fs (op fs)) >>= \case
        Left err -> throwIO err
        Right x  -> return x
      where
        assoc fs  (Left err)       = (fs,  Left err)
        assoc _fs (Right (fs', x)) = (fs', Right x)
```

We can now write components or services *against* this interface:

``` haskell
prog :: IQueue q -> IO ()
prog iq = do
  q <- iNew iq 3
  iPut iq q 0
  iPut iq q 1
  iPut iq q 2
  x <- iGet iq q
  assert (x == 0) (return ())
  sz <- iSize iq q
  assert (sz == 2) (return ())

test :: IO ()
test = prog =<< fake

deploy :: IO ()
deploy = prog real
```

When we integration test our new component we can use the `fake`
instance to make the tests fast and determinstic, while when we deploy
we use the `real` instance and because of our stateful property-based
tests we know that the fake is faithful to the real implementaton.

#### Example: file system

Edsko de Vries'
[post](https://www.well-typed.com/blog/2019/01/qsm-in-depth/) (2019).

See the [test
module](https://github.com/stevana/stateful-pbt-with-fakes/blob/main/src/Example/FileSystem/Test.hs)
for details of how the stateful property-based tests are written.

``` haskell
root :: FilePath
root = "/tmp/qc-test"

rMkDir :: Dir -> IO ()
rMkDir d = createDirectory (dirFP root d)

rOpen :: File -> IO Handle
rOpen f = openFile (fileFP root f) AppendMode

rWrite :: Handle -> String -> IO ()
rWrite h s = hPutStr h s

rClose :: Handle -> IO ()
rClose h = hClose h

rRead :: File -> IO String
rRead f = readFile (fileFP root f)
```

``` haskell
type FHandle = Var Handle

data FakeFS = F {
    dirs  :: Set Dir
  , files :: Map File String
  , open  :: Map FHandle File
  , next  :: FHandle
  }
  deriving Show

emptyFakeFS :: FakeFS
emptyFakeFS = F (Set.singleton (Dir [])) Map.empty Map.empty (Var 0)

type FakeOp a = FakeFS -> (Either PrecondFail a, FakeFS)

fMkDir :: Dir -> FakeOp ()
fMkDir d m@(F ds fs hs n)
  | d        `Set.member`    ds = (Left AlreadyExists, m)
  | parent d `Set.notMember` ds = (Left DoesNotExist, m)
  | otherwise                   = (Right (), F (Set.insert d ds) fs hs n)

fOpen :: File -> FakeOp FHandle
fOpen f m@(F ds fs hs n@(Var n_))
  | alreadyOpen   = (Left Busy, m)
  | not dirExists = (Left DoesNotExist, m)
  | fileExists    = (Right n, F ds fs hs' n')
  | otherwise     = (Right n, F ds (Map.insert f "" fs) hs' n')
  where
    hs' = Map.insert n f hs
    n'  = Var (succ n_)

    fileExists  =         f `Map.member` fs
    dirExists   = fileDir f `Set.member` ds
    alreadyOpen = f `List.elem` Map.elems hs

fWrite :: FHandle -> String -> FakeOp ()
fWrite h s m@(F ds fs hs n)
  | Just f <- Map.lookup h hs = (Right (), F ds (Map.adjust (++ s) f fs) hs n)
  | otherwise                 = (Left HandleClosed, m)

fClose :: FHandle -> FakeOp ()
fClose h (F ds fs hs n) = (Right (), F ds fs (Map.delete h hs) n)

fRead :: File -> FakeOp String
fRead f m@(F _ fs hs _)
  | alreadyOpen               = (Left Busy         , m)
  | Just s <- Map.lookup f fs = (Right s           , m)
  | otherwise                 = (Left DoesNotExist , m)
  where
    alreadyOpen = f `List.elem` Map.elems hs
```

``` haskell
data IFileSystem h = IFileSystem
  { iMkDir :: Dir -> IO ()
  , iOpen  :: File -> IO h
  , iWrite :: h -> String -> IO ()
  , iClose :: h -> IO ()
  , iRead  :: File -> IO String
  }
```

``` haskell
real :: IFileSystem Handle
real = IFileSystem
  { iMkDir = rMkDir
  , iOpen  = rOpen
  , iWrite = rWrite
  , iClose = rClose
  , iRead  = rRead
  }
```

``` haskell
fake :: IO (IFileSystem FHandle)
fake = do
  ref <- newIORef emptyFakeFS
  return IFileSystem
    { iMkDir = \d   -> updateIORef ref (fMkDir d)
    , iOpen  = \f   -> updateIORef ref (fOpen f)
    , iWrite = \h s -> updateIORef ref (fWrite h s)
    , iClose = \h   -> updateIORef ref (fClose h)
    , iRead  = \f   -> updateIORef ref (fRead f)
    }
  where
    updateIORef :: IORef FakeFS -> FakeOp a -> IO a
    updateIORef ref op =
      atomicModifyIORef' ref (\fs -> swap (op fs)) >>= \case
        Left err -> throwIO err
        Right x  -> return x
      where
        swap (x, y) = (y, x)
```

``` haskell
prog :: IFileSystem h -> IO ()
prog ifs = do
  let d = Dir ["foo"]
  iMkDir ifs d
  let f = File d "bar"
  h <- iOpen ifs f
  iWrite ifs h "baz"
  iClose ifs h
  putStrLn =<< iRead ifs f

test :: IO ()
test = prog =<< fake

deploy :: IO ()
deploy = prog real
```

#### Example: bigger system of components

The examples given above, a queue and a file system, might not seems
necessary to fake[^6] so to finish of let's sketch how the same
technique scales to a bigger system of components or services.

Imagine we have three components or services, where component *A*
depends on component *B* which depends on component *C*:

      +---+      +---+      +---+
      |   |      |   |      |   |
      | A +----->| B +----->| C |
      |   |      |   |      |   |
      +---+      +---+      +---+

Following the pattern that we did for the queue and file system example,
we'd define three interfaces:

``` haskell
data IA = ...
data IB = ...
data IC = ...
```

And the dependencies are made clear when we instantiate the interfaces:

``` haskell
iC :: IO IC       -- C has no dependencies.
iB :: IC -> IO IB -- B depends on C.
iA :: IB -> IO IA -- A depends on B.
```

The testing strategy is then as follows:

1.  Stateful and parallel test C, this gives us a fake of C which is
    contract tested;
2.  Use C fake when integration testing B;
3.  Use B fake (which uses the C fake) when testing A.

Hopefully it should be clear that this strategy scales to more
components or services[^7].

### Prior work

#### Stateful

I'd like to explain where my inspiration is coming from, because I think
it's important to note that the code I'm about to present didn't come
from thin air (even though it might look simple).

I've been thinking about this problem since the end of 2016 as can be
witnesed by my involvement in the following
[issue](https://github.com/nick8325/quickcheck/issues/139) about adding
stateful testing to Haskell's QuickCheck.

My initial attempt eventually turned into the Haskell library
`quickcheck-state-machine`.

The version below is a combination of my experience building that
library, but also inspried by:

1.  Nick Smallbone's initial
    [version](https://github.com/nick8325/quickcheck/issues/139#issuecomment-279836475) (2017)
    from that same issue. (Nick was, and I think still is, the main
    maintainer of the original QuickCheck library);

2.  John's Midlands Graduate School
    [course](https://www.cse.chalmers.se/~rjmh/MGS2019/) (2019);

3.  Edsko de Vries' "lockstep"
    [technique](https://www.well-typed.com/blog/2019/01/qsm-in-depth/)
    (2019).

XXX: I'll refer back to these when I motivate my design decisions below.

#### Parallel

The following resources where useful when me and my then colleague,
Daniel Gustafsson, first implemented parallel property-based testing in
[`quickcheck-state-machine`](https://github.com/stevana/quickcheck-state-machine/tree/master)
back in 2017.

Apart from the [*Finding Race Conditions in Erlang with QuickCheck and
PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
(2009) paper and the [*Linearizability: a correctness condition for
concurrent
objects*](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf) (1990)
paper that they reference, it was useful to have a look at the Erlang's
PropEr library, which is a (the first?) Quviq QuickCheck clone with
support for both stateful and parallel testing.

- Not property-based testing per say, but similar in that it generates
  random commands and checks linearisability is Jepsen's
  [Knossos](https://aphyr.com/posts/309-knossos-redis-and-linearizability)

## Conclusion and future work

- Stateful and parallel testing in ~400 LOC vs the 300LOC of the first
  version of QuickCheck

- How to test bigger systems in a compositional manner by reusing the
  fakes

- Translate code to other programming language paradigms, thus making it
  easier for library implementors

- repo where people can open issues and ask questions and explore
  improvements

  - examples that can be described / generated

- Having a compact code base makes it cheaper to make experimental
  changes.

- Can we use
  [`MonadAsync`](https://hackage.haskell.org/package/io-classes-1.4.1.0/docs/Control-Monad-Class-MonadAsync.html)
  and [IOSim](https://hackage.haskell.org/package/io-sim) to make
  parallel testing deterministic?

- Improving Random Generation

  - Generating Good Generators for Inductive Relations \[POPL’18\]
  - Beginner’s Luck \[POPL’17\]

- Incorporating Other Testing Techniques

  - Coverage Guided, Property Based Testing \[OOPSLA’19\]
  - Combinatorial Property-Based Testing: Do Judge a Test by its Cover
    \[ESOP’21\]

- Liveness a la quickcheck-dynamic?

- Distributed systems

  - Fault injection
    - [*Simple Testing Can Prevent Most Critical
      Failures*](https://www.usenix.org/conference/osdi14/technical-sessions/presentation/yuan)
      by Yuan et al (OSDI 2014)
    - Jepsen's knossos checker
  - Simulation testing
    - Always and sometimes combinators?

- partial order reduction: in concurrent execution, sometimes we can
  commute two operations without changing the outcome. We can explit
  this to check less histories.

Formal specification and proofs are fundamental to computer science and
have occupied minds since [Alan
Turing](https://turingarchive.kings.cam.ac.uk/publications-lectures-and-talks-amtb/amt-b-8)
(1949). Property-based testing gives us an execellent opportunity to
introduce formal specification to a lot of programmers without the
tedious and laborious formal proof part, we should cherish such eduction
opportunities.

## Acknowledgments

- Daniel for discussing fix for parallel generation issue

[^1]: Is there a source for this story? I can't remember where I've
    heard it. This short
    [biography](http://www.erlang-factory.com/conference/London2011/speakers/JohnHughes)
    gives some of the details:

    > "From 2002-2005 he led a major research project in software
    > verification, funded by the Swedish Strategic Research Foundation.
    > This led to the development of Quviq QuickCheck in Erlang."

    I believe
    [this](https://strategiska.se/forskning/genomford-forskning/ramanslag-inom-it-omradet/projekt/2010/)
    must be the project mentioned above.

[^2]: There's some room for error here from the users side, e.g. the
    user could create non-unique refererences. In a proper library one
    might want to introduce a `genSym` construct which guarantees
    uniqueness.

[^3]: So stateful property-based testing with a trivial `runReal` can be
    seen as crude version of a random path exploring "model checker".
    One could perhaps implement something closer to TLC (the model
    checker for TLA+), which enumerates all paths up to some depth, by
    using `smallcheck` rather than `QuickCheck`. If this topic interests
    you, you might also want to have a look at Gabriella Gonzalez's
    [HasCal](https://github.com/Gabriella439/HasCal).

    I don't have an example for this, but I guess one can also think of
    stateful property-based testing with a trivial `runFake` as a crude
    version of a fuzzer (without coverage guidance). For more on this
    and how to add coverage guidance, see [*Coverage guided, property
    based testing*](https://dl.acm.org/doi/10.1145/3360607) (2019).

[^4]: The parallel counter example is very similar to the ticket
    dispenser example that appears in [*Testing the hard stuff and
    staying
    sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
    (2014).

[^5]: The sequential variant of the process registry example first
    appeared in the paper [*QuickCheck testing for fun and
    profit*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
    (2007) and is also part of John's Midlands Graduate School course
    (2019). The parallel tests were introduced in [*Finding Race
    Conditions in Erlang with QuickCheck and
    PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
    (2009).

[^6]: Unless we want to test what happens when failures, such as the
    disk being full etc.
    [Research](http://www.eecg.toronto.edu/~yuan/papers/failure_analysis_osdi14.pdf)
    shows that "almost all (92%) of the catastrophic system failures are
    the result of incorrect handling of non-fatal errors explicitly
    signaled in software. \[...\] in 58% of the catastrophic failures,
    the underlying faults could easily have been detected through simple
    testing of error handling code.". Fakes make it easier to inject
    faults, but that's a story for another day.

[^7]: See the talk [Integrated Tests Are A
    Scam](https://www.youtube.com/watch?v=fhFa4tkFUFw) by J.B.
    Rainsberger for a longer presentation of this idea.
