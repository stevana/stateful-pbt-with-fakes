# Getting the most out of property-based testing

Property-based testing is a rare example of academic research that has made it
to the mainstream in less than 30 years.

Under the slogan "don't write tests, generate them" property-based testing has
gained support from a diverse group of programming language communities.

The original Haskell QuickCheck implementation has been ported to 39 languages,
according to [Wikipedia](https://en.wikipedia.org/wiki/QuickCheck), and some
languages even have multiple implementations.

In this post I'd like to explain how most of those 39+ implementations do not
help its users to fully exploit the power of property-based testing.

In order to explain what I mean by this and why I think we've ended up in this
situation, I need to give you a breif history of property-based testing first.

## The history of property-based testing

* John Hughes had a week off, just for fun project, Koen stuck his head into the office
  - https://youtu.be/x4BNj7mVTkw?t=289
  - inspired by formal methods

Property-based testing (PBT) was first introduced by Koen Claessen and John
Hughes in their paper [*QuickCheck: A Lightweight Tool for Random Testing of
Haskell
Programs*](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)
(ICFP 2000).

I think it's worth stressing the *lightweight tool* part from the paper's title,
the source code for the [first
version](https://github.com/Rewbert/quickcheck-v1) of the library is included in
the appendix of the paper and it's about 300 lines of code.

Haskell is a pure functional programming language, meaning that it's possible at
the type-level to distinguish whether a function has side-effects or not.

Probably as a result of this, the first version of QuickCheck can only test
pure functions. This shortcoming was rectified in the follow up paper [*Testing
monadic code with
QuickCheck*](https://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps) (2002) by
the same authors.

Around the same time John Hughes was applying for a major grant at the Swedish
Strategic Research Foundation, part of this process involved pitching in front
of a panel of people from industry, some person from Ericsson was on this panel
and they were interested in the tool, there was also a woman who I forgot the
name of but she is a serial entrepreneur and she encouraged John to start a
company, and the Ericsson person agreed to be a first customer, and so QuiviQ
was founded in 2006
  + is there a source for this story?
 + https://strategiska.se/forskning/genomford-forskning/ramanslag-inom-it-omradet/projekt/2010/ (2002-2006)
 + http://www.erlang-factory.com/conference/London2011/speakers/JohnHughes (2002-2005?)

* Ericsson's system was written in Erlang and was stateful and concurrent, so
  the original formulation of QuickCheck wasn't enough

* closed source Erlang version [`eqc`](http://quviq.com/documentation/eqc/)
  - sequential stateful property-based testing using a state machine model
  - parallel testing with race condition detection for free reusing the
    sequential state machine model
  - the combination of the above is what i mean by full potential and it can only
    be found in a couple of open source libraries

* [Finding Race Conditions in Erlang with QuickCheck and PULSE](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf) (ICFP 2009)
  + [Linearizability: a correctness condition for concurrent objects](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf)
  + Jepsen's knossos checker
    + also does fault injection

## A survey of (the sad state of) property-based testing libraries

Let me be clear up front that I've not used all of these libraries. My
understanding comes from reading the documentation, issue tracker and sometimes
source code.

To my best knowledge, as of March 2024, the following table summarises the
situation. Please open an
[issue](https://github.com/stevana/stateful-pbt-with-fakes/issues) if you see a
mistake.

| Library | Language | Stateful | Parallel | Notes |
| :---    | :---     | :---:    | :---:    | :---  |
| ScalaCheck | Scala | | <ul><li>- [x]</li></ul> | <ul><li>- [ ]</li></ul> | Has some support for parallel testing, but it's limited as can be witnessed by the fact that the two [examples](https://github.com/typelevel/scalacheck/tree/19af6eb656ba759980664e29ec6ae3e063021685/examples) of testing LevelDB and Redis both are sequential (`threadCount = 1`). |
| Gopter | Go | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | The README says "No parallel commands ... yet?" and there's an open [issue](https://github.com/leanovate/gopter/issues/20) from 2017. |
| Rapid | Go | <ul><li>- [x] </li></ul> | <ul><li>- [ ] </li></ul> | |
| Hypothesis | Python | <ul><li>- [x] [docs](https://hypothesis.readthedocs.io/en/latest/stateful.html)</li></ul> | <ul><li>- [ ] </li></ul> | |


* Original Haskell QuickCheck implementation still today has an open issue about
  adding state machine based test
  https://github.com/nick8325/quickcheck/issues/139

* Haskell's qsm
  + 2017-2018
  + second open source library, after https://github.com/proper-testing/proper,
    to offer parallel stateful property-based testing

* Haskell's Hedgehog, has parallel support, but the implementation has issues
  https://github.com/hedgehogqa/haskell-hedgehog/issues/104




* Erlang's PropEr, has support

* Elixir's propcheck, no support for parallel https://github.com/alfert/propcheck/issues/148

* jetCheck
  >  * Represents an action with potential side effects, for single-threaded property-based testing of stateful systems.

*  Java's QuickTheories:
  - experimental https://github.com/quicktheories/QuickTheories/issues/42

>    * Supplied commands will first be run in sequence and compared against the model,
   * then run concurrently. All possible valid end states of the system will be
   * calculated, then the actual end state compared to this.
   *
   * As the number of possible end states increases rapidly with the number of commands,
   * command lists should usually be constrained to 10 or less.
   https://github.com/quicktheories/QuickTheories/blob/a963eded0604ab9fe1950611a64807851d790c1c/core/src/main/java/org/quicktheories/core/stateful/Parallel.java#L35

* F#'s FsCheck, experimental stateful testing
  https://fscheck.github.io/FsCheck//StatefulTestingNew.html , no parallel
  testing https://github.com/fscheck/FsCheck/issues/214

* https://github.com/clojure/test.check , no support for stateful testing
  someone has implemented it in a blog post though?
  http://blog.guillermowinkler.com/blog/2015/04/12/verifying-state-machine-behavior-using-test-dot-check/

* Rust's quickcheck https://github.com/BurntSushi/quickcheck/issues/134 no
  support for stateful testing
  [issue](https://github.com/BurntSushi/quickcheck/issues/134) closed

   > Unfortunately, I don't really have the bandwidth to contemplate models of
   > property based testing that are different from how quickcheck currently
   > works.

* Rust's https://github.com/proptest-rs has a companion crate called
https://crates.io/crates/proptest-state-machine the docs say:

  > Currently, only sequential strategy is supported, but a concurrent strategy
  > is planned to be added at later point.

* C++'s RapidCheck https://github.com/emil-e/rapidcheck/issues/47 , no parallel support

* TypeScript's fast-check, has some support for race condition checking of stateful programs
  https://fast-check.dev/docs/advanced/race-conditions/

* JavaScript's jsverify, no support for stateful testing https://github.com/jsverify/jsverify/issues/148

* Swift's SwiftCheck https://github.com/typelift/SwiftCheck/issues/149 no support for stateful testing

* Ruby's [rantly](https://github.com/rantly-rb/rantly) no support for stateful testing

## Why are property-based testing libraries in a sad state and what can we do about it?

1. Not as useful as testing pure functions?
2. More difficult/work to model?
  + john hughes [says](https://youtu.be/x4BNj7mVTkw?t=898) testing this way
    requires a bit different way of thinking and you can't just give people the
    tool

3. No concise code to port?

  + Part of the original implementations spread to other languages can perhaps
    be attributed to the fact that the original implementation is small, around
    300 lines of code?

In this post:

  3. show how one can implement stateful property-based testing in 150 lines of code.
     This is the first step, I'll show how to add parallel testing in a follow up post

  4. put this technique in context of software development at large.


## QuickCheck recap (stateless property-based testing)

The original idea is that we can test some pure (or side-effect free) function
$f : A \to B$ by randomly generating its argument ($A$) and then checking that
some predicate ($P : B \to Bool$) on the output holds.

For example let's say that the function we want to test is a list reversal
function ($reverse$), then the argument we need to randomly generate is a list,
and the predicate can be anything we'd like to hold for our list reversal
function, for example we can specify that reversing the result of rerversal
gives back the original list, i.e. $reverse(reverse(xs)) \equiv xs$.

* Involution, and other common properties

* Most tutorials on property-based testing only cover testing pure functions

## Stateful property-based testing in ~150 LOC


## Parallel property-based testing in ~300 LOC

## Contract tested fakes


## Future work

Having a compact code base makes it cheaper to make experimental changes.

* Fault injection
* Simulation testing
  - Always and sometimes combinators?
* Can we use
  [`MonadAsync`](https://hackage.haskell.org/package/io-classes-1.4.1.0/docs/Control-Monad-Class-MonadAsync.html)
  and [IOSim](https://hackage.haskell.org/package/io-sim) to make parallel testing deterministic?


## See also


* https://www.well-typed.com/blog/2019/01/qsm-in-depth/
* https://www.cse.chalmers.se/~rjmh/MGS2019/
* https://github.com/nick8325/quickcheck/issues/139

* [Experiences with QuickCheck: Testing the Hard Stuff and Staying
  Sane](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quviq-testing.pdf)
  - https://www.youtube.com/watch?v=zi0rHwfiX1Q

* [How to specify it! A Guide to Writing Properties of Pure
  Functions](https://research.chalmers.se/publication/517894/file/517894_Fulltext.pdf) (2020)

  + https://www.youtube.com/watch?v=zvRAyq5wj38


* [Building on developers' intuitions to create effective property-based
  tests](https://www.youtube.com/watch?v=NcJOiQlzlXQ)
