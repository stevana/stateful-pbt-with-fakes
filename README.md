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
implementations and compare them with what used to be the state-of-the-art 15
years ago (2009). As the title already gives away, most of the libraries do not
offer their users the most advanced property-based testing features. In order to
best explain what's missing and why I think we ended up in this situation, let
me start by telling the brief history of property-based testing.

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
the sorting example you can simply generate a random input list and then compare
the output of your sorting function with the one in the standard library (which
is likely to be correct).

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

Around the same time as the second paper was published (2002), John was applying
for a major grant at the Swedish Strategic Research Foundation. A part of the
application process involved pitching in front of a panel of people from
industry. Some person from [Ericsson](https://en.wikipedia.org/wiki/Ericsson)
was on this panel and they were interested in QuickCheck. There was also a
serial entrepreneur on the panel and she encouraged John to start a company, and
the Ericsson person agreed to be a first customer, and so Quviq AB was founded
in 2006[^1] by John and Thomas Arts (perhaps somewhat surprisingly, Koen was
never involved in the company).

The first project at Ericsson that Quviq helped out testing was written in
Erlang. Unlike Haskell, Erlang is not a pure functional programming language and
on top of that there's concurrency everywhere. So even the second, monadic,
version of QuickCheck didn't turn out to be ergonomic enough for the job.

This is what motivated the closed source Quviq QuickCheck version written in
Erlang, first mentioned in the paper [*Testing telecoms software with Quviq
QuickCheck*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=b268715b8c0bcebe53db857aa2d7a95fbb5c5dbf)
(2006).

The main features of the closed source version that, as we shall see, are still
not found in many open source versions are:

  1. Sequential *stateful* property-based testing using a state machine model;
  2. *Parallel* testing with race condition detection by reusing the sequential
    state machine model.

We shall describe how these features work in detail later.

For now let's just note that *stateful* testing in its current form was first
mentioned in [*QuickCheck testing for fun and
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
libraries do not implement what used to be the state-of-the-art in 2009.

Many lack stateful testing via state machines (2007) and most lack parallel
testing support (2009).

Often users of the libraries have opened tickets asking for these features,
often these tickets have stayed open for years without any progress.

Furthermore it's not clear to me whether all libraries that support stateful
testing can be generalised to parallel testing without a substantial redesign of
their APIs. I don't think there's a single example of a library to which
parallel testing was added later, rather than designed for from the start.

### Why are property-based testing libraries in such a sad state?

Here are three reasons I've heard from John:

1. The stateful and parallel testing featurs are not as useful as testing pure
   functions. This is what John told me when I asked him why these features
   haven't taken off in Haskell (BobKonf 2017);

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
says to be true and I believe he acts with the best intentions.

I do agree that separating pure from side-effectful code is certainly good
practice in any programming language and that you can get far by merely
property-based testing those pure fragments. However I also do think that
stateful and parallel testing is almost equally important for many non-trivial
software systems. Most systems in industry will have some database, stateful
protocol or use concurrent datastructures, which all benefit from these
features.

Regarding formal specification requiring a special way of thinking and therefor
training, I believe this is a correct assessment, but I also believe that this is
already true for property-based testing of pure functions.

Formal specification and proofs are fundamental to computer science and have
occupied minds since [Alan
Turing](https://turingarchive.kings.cam.ac.uk/publications-lectures-and-talks-amtb/amt-b-8)
(1949). Property-based testing gives us an execellent opportunity to introduce
formal specification to a lot of programmers without the formal proof part.

John has written papers and given talks on the topic of making property-based
testing of pure functions more accessible to programmers:

* [*How to specify it! A Guide to Writing Properties of Pure
  Functions*](https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf)
  (2020)

* [Building on developers' intuitions to create effective property-based
  tests](https://www.youtube.com/watch?v=NcJOiQlzlXQ) (2019)

Can we do the same for stateful and parallel testing? I think stateful
specifications are not necessarily always harder than specifications for pure
functions.

The experience reports that we've already mentioned above, usually contain some
novelty (which warrents a new paper) rather than general advice which can be
done with the vanilla stateful and parallel testing features.

Regarding keeping the source closed helping with adoption, I think this is
perhaps the most controversial point that John makes.

If we try to see it from John's perspective, how else would an academic get
funding to work on tooling (which typically isn't reconginised as doing
research), feedback from industry, or be able to hire people? Surely, one cannot
expect research funding agencies to pay for this?

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

So here we are 15-18 years after the first papers that introduced stateful and
parallel testing, dispite the best efforts of everyone involved, and we still
don't have these features in most property-based testing libraries, even though
these features are clearly useful.

Personally I got quite sad when I saw that stateful testing was
[called](https://lobste.rs/s/1aamnj/property_testing_stateful_code_rust#c_jjs27f)
harder to learn and more heavyweight than an ad hoc approximation of it using
vanilla property-based testing.

I think this is evidence of the fact that people don't fully understand the full
benefits of parallel testing. While it's true that stateful testing adds another
layer or API that you have to learn, but from this sequential model we can
derive parallel tests by adding two lines of code. Can't blame them when only
4/27 libraries show how to do this.

### What can we do about it?

Even if John is right and that keeping it closed source has helped adoption in
industry, it has not helped open source adoption. Or perhaps rather, it's
unlikely that a company that pays for a licence in Erlang would then go and port
the library in another language.

I like to think that part of the original QuickCheck library's success in
spreading to so many other languages can be attributed to the fact that it is
small, around 300 lines of code, and is part of the original paper.

Perhaps if the code for stateful and parallel testing was as small and was
provided in the papers, then we would have more libraries supporting those
features by now?

Regarding specifications requring a different way of thinking that needs
training, perhaps we can avoid this by not using state machines as the basis for
the specifications, but rather reuse techniques that programmers are already
familar with?

## Synthesis

In order to test the above hypothesis, I'd like to spend the rest of this post
as follows:

  1. show how one can implement stateful property-based testing in 150 lines of code.

  2. add parallel testing in ~300 lines of code

  3. make specifications simpler using fakes, and put this technique in context
     of software development at large.

Before we get started with stateful testing, let's first recap what vanilla
(stateless) property-based testing does.

### Property-based testing recap

The original idea is that we can test some pure (or side-effect free) function
$f : A \to B$ by randomly generating its argument ($A$) and then checking that
some relation, $R : A \to B \to Bool$, between the input and output holds.

For example let's say that the function we want to test is a list reversal
function ($reverse$), then the argument we need to randomly generate is a list,
and the relation can be anything we'd like to hold for our list reversal
function, for example we can specify that reversing the result of rerversal
gives back the original list, i.e. $reverse(reverse(xs)) \equiv xs$.

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
helpful analogy for now).

XXX: https://fsharpforfunandprofit.com/posts/property-based-testing-2/

* Most tutorials on property-based testing only cover testing pure functions

### Stateful property-based testing in ~150 LOC

Having recalled how vanilla property-based testing works, let's now build a
module on top which allows us to do stateful testing.

Before we do so, I'll give some reasons why developing such a module make sense,
how it works at a high-level and what my sources of inspiration are.

#### Motivation

In order to explain why stateful property-based testing is needed, it's perhaps
helpful to have a look at how it's different from vanilla property-based testing.

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
subsequent changes via API calls to that page need to include that UUID, etc.

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

* Trait, type class, protocol type, module signatures

##### Generating and shrinking
##### Running and assertion checking

#### Example: counter

#### Example: array-based queue

The queue example from [*Testing the hard stuff and staying
sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
(2014)

##### SUT

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

##### Model / fake

```haskell
type State = Map (Var Queue) FQueue

data FQueue = FQueue
  { fqElems :: [Int]
  , fqSize  :: Int
  }
  deriving Show

data Err = QueueDoesNotExist | QueueIsFull | QueueIsEmpty
  deriving (Eq, Show)

fnew :: Int -> State -> Return Err (State, Var Queue)
fnew sz s =
  let
    v = Var (Map.size s)
  in
    return (Map.insert v (FQueue [] sz) s, v)

fput :: Var Queue -> Int -> State -> Return Err (State, ())
fput q i s
  | q `Map.notMember` s = Precondition QueueDoesNotExist
  | length (fqElems (s Map.! q)) >= fqSize (s Map.! q) = Precondition QueueIsFull
  | otherwise = return (Map.adjust (\fq -> fq { fqElems = fqElems fq ++ [i] }) q s, ())

fget :: Var Queue -> State -> Return Err (State, Int)
fget q s
  | q `Map.notMember` s        = Precondition QueueDoesNotExist
  | null (fqElems (s Map.! q)) = Precondition QueueIsEmpty
  | otherwise = case fqElems (s Map.! q) of
      [] -> error "fget: impossible, we checked that it's non-empty"
      i : is -> return (Map.adjust (\fq -> fq { fqElems = is }) q s, i)

fsize :: Var Queue -> State -> Return Err (State, Int)
fsize q s
  | q `Map.notMember` s = Precondition QueueDoesNotExist
  | otherwise           = return (s, length (fqElems (s Map.! q)))
```

##### Testing

```haskell
instance StateModel State where

  initialState = Map.empty

  type Reference State = Queue
  type Failure State = Err

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

  -- These are FFI bindings that call the C code.
  runReal (New sz)  = New_  <$> new sz
  runReal (Put q i) = Put_  <$> put q i
  runReal (Get q)   = Get_  <$> get q
  runReal (Size q)  = Size_ <$> size q

  runCommandMonad _ = id

prop_queue :: Commands State -> Property
prop_queue cmds = monadicIO $ do
  _ <- runCommands cmds
  assert True
```

+ regression tests?

#### Example: process registry

This example comes from the paper [*QuickCheck testing for fun and
profit*](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=5ae25681ff881430797268c5787d7d9ee6cf542c)
(2007) and is also part of John's Midlands Graduate School course (2019).

#### Example: file system

+ proper coverage?

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

### Parallel property-based testing in ~300 LOC

Let's now turn our focus to parallel property-based testing.

Before showing you the implementation, let me start off by giving a bit of
motivation, a high-level sketch of how it works, and mentioning some prior work
that I've stolen ideas from.

#### Motivation

Typically debugging buggy concurrent code is not fun. The main reason for this
is that the threads interleave in different ways between executions, making it
hard to reproduce the bug.

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

![](./images/sequence-diagram.svg){ width=300px }

Here we see that the first and second thread concurrently increment, the first
thread then reads the counter concurrently with the second thread's increment
that's still going on. The second thread's increment finishes and a third thread
does a read which is concurrent with the first thread's read.

We can abstract away the arrows and merely focus on the intervals of the
commands:

![](./images/history-from-sequence-diagram.svg){ width=400px }

If we rotate the intervals we get the concurrent history:

![](./images/concurrent_counter.svg){ width=300px }

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

![](./images/concurrent_counter_get_1_3.svg){ width=300px }

and the other interleaving corresponds to the sequential history `< incr 1, incr
2, get, get >`:

![](./images/concurrent_counter_get_3_3.svg){ width=300px }

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

* PropEr
* qsm

* Not property-based testing per say, but similar in that it generates random
  commands and checks linearisability is Jepsen's Knossos

* Linearizability paper
* Erlang

#### Implementation

##### Parallel program generation and shrinking

##### Linearisability checking

##### Parallel running

#### Example: ticket dispenser

[*Testing the hard stuff and staying
sane*](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
(2014)

#### Example: process registry

The parallel tests for the process registry was introduced in [*Finding Race
Conditions in Erlang with QuickCheck and
PULSE*](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)
(2009)

### Integration testing with contract tested fakes

* Vanilla property-based testing generates unit tests, what about stateful and
  parallel property-based testing?

* Fake instead of state machine spec is not only easier for programmers
  unfamilar with formal specification, it's also more useful in that the fake
  can be used in integration tests with components that depend on the SUT

* https://martinfowler.com/bliki/ContractTest.html
* Edsko's lockstep https://www.well-typed.com/blog/2019/01/qsm-in-depth/
* [Integrated Tests Are A Scam](https://www.youtube.com/watch?v=fhFa4tkFUFw) by J.B. Rainsberger
* Queue example again?

## Future work

* Translate code to other programming language paradigms, thus making it easier
  for library implementors

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
    + Jepsen's knossos checker
  - Simulation testing
    + Always and sometimes combinators?


[^1]: Is there a source for this story? I can't remember where I've heard it.
    This short
    [biography](http://www.erlang-factory.com/conference/London2011/speakers/JohnHughes)
    gives some of the details:

    > From 2002-2005 he led a major research project in software verification,
    > funded by the Swedish Strategic Research Foundation. This led to the
    > development of Quviq QuickCheck in Erlang.

    I believe [this](https://strategiska.se/forskning/genomford-forskning/ramanslag-inom-it-omradet/projekt/2010/)
    must be the project mentioned above.
