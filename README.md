# Stateful property-based testing in 150 LOC

Property-based testing is a rare example of academic research that has made it
to the mainstream in less than 30 years.

Under the slogan "don't write tests, generate them" property-based testing has
gained support from a diverse group of programming language communities.

The original Haskell QuickCheck implementation has been ported to 39 languages,
according to Wikipedia, and some languages even have multiple implementations.

In this post I'd like to explain how most of those 39+ implementations do not
help its users to fully exploit the power of property-based testing.

In order to explain why I think we've ended up in this situation, I'll give you
a breif history of property-based testing.

In this post:

  3. show how one can implement stateful property-based testing in 150 lines of code.
     This is the first step, I'll show how to add parallel testing in a follow up post

  4. put this technique in context of software development at large.

## History

* John Hughes and Koen Claessen write QuickCheck
  + https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf (ICFP 2000)

  + https://github.com/Rewbert/quickcheck-v1

* *Testing monadic code with QuickCheck*
  https://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps (2002)

* John Hughes was applying for a grant at the Swedish Strategic Research
  Foundation, part of this process involved pitching in front of a panel of
  people from industry, some person from Ericsson was on this panel and they
  were interested in the tool, there was also a woman who I forgot the name of
  but she is a serial entrepreneur and she encouraged John to start a company,
  and the Ericsson person agreed to be a first customer, and so QuiviQ was
  founded
  https://strategiska.se/forskning/genomford-forskning/ramanslag-inom-it-omradet/projekt/2010/
  http://www.erlang-factory.com/conference/London2011/speakers/JohnHughes

* Ericsson's system was written in Erlang and was stateful and concurrent, so
  the original formulation of QuickCheck wasn't enough

* closed source Erlang version [`eqc`](http://quviq.com/documentation/eqc/)
  - sequential stateful property-based testing using a state machine model
  - parallel testing with race condition detection for free reusing the
    sequential state machine model
  - the combination of the above is what i mean by full potential and it can only
    be found in a couple of open source libraries

* [Finding Race Conditions in Erlang with QuickCheck and PULSE](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf)

* [Linearizability: a correctness condition for concurrent objects](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf)
* Jepsen's knossos checker
  + also does fault injection

* Original Haskell QuickCheck implementation still today has an open issue about
  adding state machine based test
  https://github.com/nick8325/quickcheck/issues/139

* qsm
  + 2017-2018
  + second open source library, after https://github.com/proper-testing/proper,
    to offer parallel stateful property-based testing

## A survey of property-based testing libraries

* https://github.com/leanovate/gopter , readme says "No parallel commands ... yet?"
  + https://github.com/leanovate/gopter/issues/20
* https://github.com/flyingmutant/rapid/ no support for parallel

* ScalaCheck
  + https://github.com/typelevel/scalacheck/blob/main/core/shared/src/main/scala/org/scalacheck/commands/Commands.scala#L181

    /** A property that can be used to test this [[Commands]] specification.
     *
     * The parameter `threadCount` specifies the number of commands that might be executed in parallel. Defaults to one,
     * which means the commands will only be run serially for the same [[Sut]] instance. Distinct [[Sut]] instances might
     * still receive commands in parallel, if the [[Test.Parameters.workers]] parameter is larger than one. Setting
     * `threadCount` higher than one enables ScalaCheck to reveal thread-related issues in your system under test.
     *
     * When setting `threadCount` larger than one, ScalaCheck must evaluate all possible command interleavings (and the
     * end [[State]] instances they produce), since parallel command execution is non-deterministic. ScalaCheck tries out
     * all possible end states with the [[Command.postCondition]] function of the very last command executed (there is
     * always exactly one command executed after all parallel command executions). If it fails to find an end state that
     * satisfies the postcondition, the test fails. However, the number of possible end states grows rapidly with
     * increasing values of `threadCount`. Therefore, the lengths of the parallel command sequences are limited so that
     * the number of possible end states don't exceed `maxParComb`. The default value of `maxParComb` is 1000000.
     */

  + leveldb and redis examples use threadCount = 1
     https://github.com/typelevel/scalacheck/tree/19af6eb656ba759980664e29ec6ae3e063021685/examples

* Python's https://hypothesis.readthedocs.io/en/latest/stateful.html , no parallel support

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

* Haskell's Hedgehog, has parallel support, but the implementation has issues
  https://github.com/hedgehogqa/haskell-hedgehog/issues/104

## Why isn't stateful and parallel testing more widespread?

1. Not as useful as testing pure functions?
2. More difficult/work to model?
3. No concise code to port?


## QuickCheck recap (stateless property-based testing)

The original idea is that we can test some pure (or side-effect free) function
$f : A -> B$ by randomly generating its argument ($A$) and then checking that
some predicate ($P : B -> Bool$) on the output holds.

For example let's say that the function we want to test is a list reversal
function ($reverse), then the argument we need to randomly generate is a list,
and the predicate can be anything we'd like to hold for our list reversal
function, for example we can specify that reversing the result of rerversal
gives back the original list, i.e. $reverse(reverse(xs)) == xs$.

* Involution, and other common properties

* Most tutorials on property-based testing only cover testing pure functions



Part of it's spread to other languages can no doubt be attributed to the fact
that the original implementation is small, around 300 lines of code.


This of course wasn't an accident, we don't need to look further than the title
of the original paper: "QuickCheck: *A Lightweight Tool* for Random Testing of
Haskell Programs" (mine emphasis).

For those who haven't heard of property-based testing it can perhaps best be
explained by the slogan: "don't write tests, generate them".

* example


## Stateful property-based testing



## See also

* https://en.wikipedia.org/wiki/QuickCheck

* https://www.well-typed.com/blog/2019/01/qsm-in-depth/
* https://www.cse.chalmers.se/~rjmh/MGS2019/
* https://github.com/nick8325/quickcheck/issues/139

* [Experiences with QuickCheck: Testing the Hard Stuff and Staying
  Sane](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quviq-testing.pdf)
  - https://www.youtube.com/watch?v=zi0rHwfiX1Q
