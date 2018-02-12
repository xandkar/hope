[![Build Status](https://travis-ci.org/ibnfirnas/hope.svg?branch=master)](https://travis-ci.org/ibnfirnas/hope)

Hope
====

A quest for a "standard" library with uniform, composable abstractions.

Originally motivated by a desire for an error monad and generic option type
operations, and stood for _Higher Order Programming in Erlang_. Soon after, I
wished all standard containers used consistent conventions and protocols (such
as consistent accessor names, argument positioning rules and expression of
semantics with option and result types).

Here lies an experiment to see what something like that could look like. As all
proper experiments should, this one is used daily in production projects (hence
the high-ish version number, 'cause semver).


Conventions
-----------

I entertain any forward-thinking library design ideas, but more than anything
else, these are influenced by Jane Street's Core of the OCaml world.

- A module per data type implementation
- Name of the module is the name of the type
- Inside the module, the type it implements is always named t(..), such as:
  `hope_foo:t()`, _not_ `hope_foo:foo()`
- t(..) is always the first argument
- Names of private records _may_ be short, such as: `#foo{}` or `#t{}` (Though
  I'm second-guessing this idea, since seeing `{t, ..}` in stack traces is less
  than helpful. I'm considering requiring fully-qualified names for all record
  definitions and maybe short-handing what would've been `#t{..}` as
  `-define(T, ?MODULE). -record(?T, {..}).`, which may be a bit ugly. Still
  thinking...)
- Names of public records _must_ be fully qualified, such as: `#hope_module_record{}`
- Names of all modules _must_ be fully qualified, such as: `hope_module` (this
  should go without saying, but just to be sure...)
- Keep the number of (anonymous) arguments "reasonably" low:
    + up to 3 is normal
    + 4 is suspicious but may be reasonable
    + 5 is _very_ suspicious and probably unnecessary
    + more than 5 is unacceptable, so consider reducing by:
        1. revising abstractions, or, if not practical
        2. creating a public record specifically for the purpose of passing
           many arguents, which simulates labeled arguments. For an example see
           https://github.com/ibnfirnas/oauth1_core where I used that technique
           extensively (especially in oauth1_server.erl)


Abstractions
------------

### Monads

A class of burritos, used for expressing sequences of operations on some data
type.  Defined in `hope_gen_monad`, implemented as:

- `hope_result`: for composition of common functions returning
  `{ok, Val} | {error, Reason}`. An alternative to exceptions, which makes the
  error conditions apparent in the spec/signature. Analogous to Haskell's
  `Data.Either a b`, Jane Street Core's (OCaml) `('a, 'b) Result.t`, Rust's
  `Result<T, E>`
- `hope_option`: for expressing and composing the intention that the value may
  or may not be available. An alternative to the common `undefined` (which is
  equivalent to the dreaded `null`). Analogous to ML's (SML, OCaml, etc)
  `'a Option.t`, Rust's `Option<T>` and Haskell's `Data.Maybe a` [1].


### Containers

A class of abstract data types to which we have exclusive access and can put
things in and take them out. See issue #9

- Operations on all abstract types of containers _should_ share a common lexicon
- Concrete implementations of an abstract data type _must_ be swapable

#### Dictionary

Defined in `hope_gen_dictionary`, implemented as:

- `hope_kv_list`. Equivalent to orddict/proplist. Operations implemented with
  BIFs from `lists` module, where possible

TBD:
- `hope_hash_tbl`. API around stdlib's `dict`
- `hope_gb_dict`. API around stdlib's `gb_trees`

#### Set

TBD:
- `hope_hash_set`. API around stdlib's `sets`
- `hope_gb_set`. API around stdlib's `gb_sets`

#### Queue

TBD

Should include both FIFO (queue) and LIFO (stack), so that user can swap if a
different order is desired.

Should we attempt to include priority queues or make them a separate abstract
type?

#### Sequence

TBD

Not yet defined and only partially implemented as:

- `hope_list`


### Resources

A class of abstract systems to which we share access with an unknown number of
users and can make requests to perform operations which may not get done for
any number of reasons.

#### Storage

TBD

See issue #11


[1]: http://en.wikipedia.org/wiki/Option_type
